{-# LANGUAGE OverloadedStrings #-}

module BroScore.DB where 

import Control.Monad
import Control.Exception

import Control.Concurrent
import Control.Concurrent.MVar

import qualified Data.ByteString.Lazy.Char8  as B
import qualified Data.ByteString.Base16.Lazy as Bhex
import qualified Data.Map                    as M

import Data.Time.Clock

import Data.Binary

import Crypto.Random
import Data.Digest.Pure.SHA

import Database.HDBC
import Database.HDBC.PostgreSQL

import Network.HTTP.Types.Status

import BroScore.Types

pgEncode = ("\\x" `B.append`) . Bhex.encode

pgDecode = fst . Bhex.decode . B.drop 2

gmpDecode SqlNull = 0
gmpDecode x       = (decode  . pgDecode . fromSql) x

gmpEncode = toSql . pgEncode . encode

prepare_mkUserS :: Connection -> IO Statement
prepare_mkUserS = flip prepare q
    where q = "insert into \"user\" (\"username\", \"password_hash\", \"password_salt\") values (?, ?, ?) returning (\"id\");"

prepare_mkPersonS :: Connection -> IO Statement
prepare_mkPersonS = flip prepare q
    where q = "insert into \"person\" (\"name\", \"context_id\") values (?, ?) returning (\"id\");"

prepare_mkContextS :: Connection -> IO Statement
prepare_mkContextS = flip prepare q
    where q = "insert into \"context\" (\"name\", \"description\") values (?, ?) returning (\"id\");"

prepare_mkScoreIncS :: Connection -> IO Statement
prepare_mkScoreIncS = flip prepare q
    where q = "insert into \"score_inc\" (\"user_id\", \"person_id\", \"activity\", \"delta\", \"init_time\", \"range\") values (?, ?, ?, ?, ?, ?);"

prepare_updateUserS :: Connection -> IO Statement
prepare_updateUserS = flip prepare q
    where q = "update \"user\" set \"votes\" = ?, \"impact\" = ? where \"id\" = ?;"

prepare_updatePersonS :: Connection -> IO Statement
prepare_updatePersonS = flip prepare q
    where q = "update \"person\" set \"score\" = ?, \"hits\" = ? where \"id\" = ?;"

prepare_checkPasswordS :: Connection -> IO Statement
prepare_checkPasswordS = flip prepare q
    where q = "select \"password_hash\", \"password_salt\" from \"user\" where \"id\" = ?;"

prepare_updatePasswordS :: Connection -> IO Statement
prepare_updatePasswordS = flip prepare q
    where q = "update \"user\" set \"password_hash\" = ?, \"password_salt\" = ? where \"id\" = ?;"

initUsers :: Connection -> IO (M.Map B.ByteString (MVar User))
initUsers conn = quickQuery' conn "select \"id\", \"username\", \"votes\", \"impact\" from \"user\";" [] >>= mkUmap
    where mkUmap = ((return . M.fromList) =<<) . mapM ((\(x,y) -> newMVar y >>= return . (,) x) . mkUser)
          mkUser (d:u:v:i:[]) = ((fromSql u), User (fromSql d) (fromSql u) Nothing (gmpDecode  v) (gmpDecode i))
          mkUser  x           = error $ "initUsers: Got bad row from DB when creating user: " ++ show x

initContexts :: Connection -> IO (M.Map B.ByteString (MVar (Context, M.Map B.ByteString (MVar Person))))
initContexts conn = quickQuery' conn "select \"id\", \"name\", \"description\" from \"context\";" [] >>= mkCmap
    where mkCmap = ((return . M.fromList) =<<) . mapM (((\(x,y) -> newMVar y >>= return . (,) x) =<<) . mkContext)
          mkContext (i:n:d:[]) = (mkPMap i) >>= (\pmap -> return ((fromSql n), ((Context (fromSql i) (fromSql n) (fromSql d)), pmap)))
          mkContext x = error $ "initContexts: Got bad row from DB when creating context: " ++ show x
          mkPMap i = quickQuery' conn "select \"id\", \"name\", \"score\", \"hits\" from \"person\" where \"context_id\" = ?" [i]
                      >>= mapM ((\(x,y) -> newMVar y >>= return . (,) x) . mkPerson) >>= (return . M.fromList)
          mkPerson (d:n:s:i:[]) = ((fromSql n), Person (fromSql d) (fromSql n) (gmpDecode s) (gmpDecode i) M.empty)
          mkPerson x            = error $ "initContexts: Got bad row from DB when createing person: " ++ show x

initBro :: String -> IO BroState
initBro pwd = do
    conn <- connectPostgreSQL $ "dbname=broscore user=broscore application_name=broscore_backend password=" ++ pwd
    mkUserP <- prepare_mkUserS conn
    mkPersonP <- prepare_mkPersonS conn
    mkContextP <- prepare_mkContextS conn
    mkScoreIncP <- prepare_mkScoreIncS conn
    updateUserP <- prepare_updateUserS conn
    updatePersonP <- prepare_updatePersonS conn
    checkPasswordP <- prepare_checkPasswordS conn
    updatePasswordP <- prepare_updatePasswordS conn
    umap <- newMVar =<< initUsers conn
    cmap <- newMVar =<< initContexts conn
    return $ BroState { broconn = conn
                       ,mkUserS = mkUserP
                       ,mkPersonS = mkPersonP
                       ,mkContextS = mkContextP
                       ,mkScoreIncS = mkScoreIncP
                       ,updateUserS = updateUserP
                       ,updatePersonS = updatePersonP
                       ,checkPasswordS = checkPasswordP
                       ,updatePasswordS = updatePasswordP
                       ,brousers = umap
                       ,brocontexts = cmap
                       }

initBro' = initBro "microsoftwouldntdothis"

mkUser :: BroState -> B.ByteString -> B.ByteString -> IO (Either Status B.ByteString)
mkUser s u p = bracketOnError (takeMVar $ brousers s) ((const (return (Left status500)) =<<) . putMVar (brousers s)) checkDup
    where checkDup um = if M.member u um then putMVar (brousers s) um >> return (Left status409) else mkUser' um
          mkUser'  um = do
            epool <- createEntropyPool
            let rng = (cprgCreate epool :: SystemRNG)
            let salt = (B.fromStrict . fst . cprgGenerate 32) rng
            withTransaction (broconn s) $ const $ do
                execute (mkUserS s) [(toSql u), ((toSql . pgEncode . bytestringDigest . sha512) (p `B.append` salt)), (toSql $ pgEncode salt)]
                [[i]] <- fetchAllRows' (mkUserS s)
                let ekey = (B.fromStrict . fst . cprgGenerate 32) rng
                newMVar (User (fromSql i) u (Just ekey) 0 0) >>= (\u' -> putMVar (brousers s) (M.insert u u' um))
                return $ Right ekey

mkPerson :: BroState -> B.ByteString -> B.ByteString -> IO (Maybe Status)
mkPerson s c n = readMVar (brocontexts s) >>= checkCtx . M.lookup c
    where checkCtx Nothing    = return $ Just status404
          checkCtx (Just cmm) = bracketOnError (takeMVar cmm) ((const (return (Just status500)) =<<) . putMVar cmm) $ checkPerson cmm
          checkPerson cmm (ct, pm) = if M.member n pm then putMVar cmm (ct, pm) >> return (Just status409) else mkPerson' cmm ct pm
          mkPerson' cmm ct pm = withTransaction (broconn s) $ const $ do
                execute (mkPersonS s) [(toSql n), (toSql (contextId ct))]
                [[i]] <- fetchAllRows' (mkPersonS s)
                newMVar (Person (fromSql i) n 0 0 M.empty) >>= (\p' -> putMVar cmm (ct, M.insert n p' pm))
                return Nothing

mkContext :: BroState -> B.ByteString -> B.ByteString -> IO (Maybe Status)
mkContext s c d = bracketOnError (takeMVar $ brocontexts s) ((const (return (Just status500)) =<<) . putMVar (brocontexts s)) checkDup
    where checkDup cm = if M.member c cm then putMVar (brocontexts s) cm >> return (Just status409) else mkContext' cm
          mkContext' cm = withTransaction (broconn s) $ const $ do
                execute (mkContextS s) [(toSql c), (toSql d)]
                [[i]] <- fetchAllRows' (mkContextS s)
                newMVar (Context (fromSql i) c d, M.empty) >>= (\c' -> putMVar (brocontexts s) (M.insert c c' cm))
                return Nothing

authUser :: BroState -> B.ByteString -> B.ByteString -> IO (Either Status B.ByteString)
authUser s u p = readMVar (brousers s) >>= checkExists . M.lookup u
    where checkExists Nothing = return $ Left status404
          checkExists (Just um) = bracketOnError (takeMVar um) ((const (return (Left status500)) =<<) . putMVar um) $ checkPassword um
          checkPassword um u' = withTransaction (broconn s) $ const $ do
                execute (checkPasswordS s) [(toSql $ userId u')]
                [[ph, ps]] <- fetchAllRows' (checkPasswordS s)
                if ((bytestringDigest . sha512) . (`B.append` ((pgDecode . fromSql) ps))) p == ((pgDecode . fromSql) ph)
                then do
                        epool <- createEntropyPool
                        let rng = (cprgCreate epool :: SystemRNG)
                        let ekey = (B.fromStrict . fst . cprgGenerate 32) rng
                        putMVar um (u' { ephemeralKey = Just ekey})
                        return (Right ekey)
                else putMVar um u' >> return (Left status401)

updatePassword :: BroState -> B.ByteString -> B.ByteString -> B.ByteString -> IO (Either Status B.ByteString)
updatePassword s u k p = readMVar (brousers s) >>= checkExists . M.lookup u
    where checkExists Nothing = return $ Left status404
          checkExists (Just um) = bracketOnError (takeMVar um) ((const (return (Left status500)) =<<) . putMVar um) $ checkKey um
          checkKey um u' = case (ephemeralKey u') of Nothing -> (putMVar um u' >> return (Left status401))
                                                     Just k' -> if k == k' then updatePassword' um u'
                                                                           else putMVar um u' >> return (Left status401)
          updatePassword' um u' = do
            epool <- createEntropyPool
            let rng = (cprgCreate epool :: SystemRNG)
            let salt = (B.fromStrict . fst . cprgGenerate 32) rng
            withTransaction (broconn s) $ const $ do
                execute (updatePasswordS s) [((toSql . pgEncode . bytestringDigest . sha512) (p `B.append` salt)), (toSql $ pgEncode salt), (toSql $ userId u')]
                let ekey = (B.fromStrict . fst . cprgGenerate 32) rng
                putMVar um (u' {ephemeralKey = Just ekey})
                return (Right ekey)

freezeBro :: BroState -> IO ()
freezeBro s = withTransaction (broconn s) $ const $ freezeUsers >> freezeContexts
    where freezeUsers = readMVar (brousers s) >>= mapM_ ((freezeUser =<<) . readMVar . snd) . M.toList
          freezeUser u = execute (updateUserS s) [(gmpEncode $ votes u), (gmpEncode $ impact u), (toSql $ userId u)]
          freezeContexts = readMVar (brocontexts s) >>= mapM_ (freezeContext . snd) . M.toList
          freezeContext = ((mapM_ (freezePerson . snd) . M.toList . snd) =<<) . readMVar
          freezePerson p = bracketOnError (takeMVar p) (putMVar p) $ \p' -> do
                execute (updatePersonS s) [(gmpEncode $ score p'), (gmpEncode $ hits p'), (toSql $ personId p')]
                (mapM_ (freezeScoreInc (toSql $ personId p') . snd) . M.toList . events) p'
                putMVar p (p' {events = M.empty})
          freezeScoreInc pid sinc = do
                sinc' <- readMVar sinc
                t <- getCurrentTime
                execute (mkScoreIncS s) [((toSql . user) sinc')
                                        ,pid
                                        ,(gmpEncode $ activity sinc')
                                        ,(gmpEncode $ delta sinc')
                                        ,(toSql $ initTime sinc')
                                        ,(toSql $ diffUTCTime t (initTime sinc'))
                                        ]
