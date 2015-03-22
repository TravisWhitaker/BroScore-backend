module BroScore.Query where

import Control.Exception
import Control.Monad

import Control.Concurrent
import Control.Concurrent.MVar

import qualified Data.ByteString.Lazy.Char8 as B

import qualified Data.Map as M

import Data.Time.Clock

import Network.HTTP.Types.Status

import BroScore.Types

getContexts :: BroState -> IO [Context]
getContexts bs = readMVar (brocontexts bs) >>= mapM (liftM fst . readMVar . snd) . M.toList

getPeople :: BroState -> B.ByteString -> IO [Person]
getPeople bs cn = readMVar (brocontexts bs) >>= checkExists . M.lookup cn
    where checkExists Nothing = return []
          checkExists (Just cm) = readMVar cm >>= mapM (readMVar . snd) . M.toList . snd

getPerson :: BroState -> B.ByteString -> B.ByteString -> IO (Maybe Person)
getPerson bs cn pn = readMVar (brocontexts bs) >>= checkContextExists . M.lookup cn
    where checkContextExists Nothing = return Nothing
          checkContextExists (Just cm) = readMVar cm >>= checkPersonExists . M.lookup pn . snd
          checkPersonExists Nothing = return Nothing
          checkPersonExists (Just pm) = liftM Just $ readMVar pm

getUser :: BroState -> B.ByteString -> B.ByteString -> IO (Either Status User)
getUser bs un k = readMVar (brousers bs) >>= checkExists . M.lookup un
    where checkExists Nothing = return $ Left status404
          checkExists (Just um) = readMVar um >>= (\u' -> case (ephemeralKey u') of Nothing -> return $ Left status401
                                                                                    Just k' -> if k == k' then return $ Right u'
                                                                                                          else return $ Left status403)

checkAuth :: BroState -> B.ByteString -> B.ByteString -> IO (Maybe Status)
checkAuth bs un k = readMVar (brousers bs) >>= checkExists . M.lookup un
    where checkExists Nothing = return $ Just status404
          checkExists (Just um) = readMVar um >>= (\u' -> case (ephemeralKey u') of Nothing -> return $ Just status401
                                                                                    Just k' -> if k == k' then return $ Nothing
                                                                                                          else return $ Just status403)

vote :: BroState -> B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> Integer -> IO (Maybe Status)
vote s u k c p v = readMVar (brousers s) >>= checkUserExists . M.lookup u
    where checkUserExists Nothing = return $ Just status404
          checkUserExists (Just um) = bracketOnError (takeMVar um) ((const (return (Just status500)) =<<) . putMVar um) $ checkKey um
          checkKey um u' = case (ephemeralKey u') of Nothing -> putMVar um u' >> return (Just status401)
                                                     Just k' -> if k == k' then findContext um u'
                                                                           else putMVar um u' >> return (Just status403)
          findContext um u' = readMVar (brocontexts s) >>= checkContextExists um u' . M.lookup c
          checkContextExists um u' Nothing = putMVar um u' >> return (Just status404)
          checkContextExists um u' (Just cm) = readMVar cm >>= checkPersonExists um u' . M.lookup p . snd
          checkPersonExists um u' Nothing = putMVar um u' >> return (Just status404)
          checkPersonExists um u' (Just pm) = bracketOnError (takeMVar pm) ((const (return (Just status500)) =<<) . putMVar pm) $ vote' um u' pm
          vote' um u' pm p' = do
                e' <- case M.lookup u (events p') of Nothing -> getCurrentTime >>= (\t -> newMVar $ ScoreInc (userId u') (abs v) v t)
                                                     Just sm -> takeMVar sm >>= (\s -> putMVar sm $ s {activity = (activity s + (abs v)), delta = (delta s + v)}) >> return sm
                let u'' = u' {votes = (votes u' + (abs v)), impact = (impact u' + (abs v))}
                let p'' = p' {score = (score p' + v), hits = (hits p' + (abs v)), events = M.insert u e' (events p')}
                putMVar um u''
                putMVar pm p''
                return Nothing
