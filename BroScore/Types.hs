module BroScore.Types where

import Control.Monad

import Control.Concurrent.MVar

import Data.List

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map                   as M

import Data.Time.Clock

import Data.Aeson

import Database.HDBC
import Database.HDBC.PostgreSQL

data User = User {
    userId       :: Integer
   ,userName     :: B.ByteString
   ,ephemeralKey :: Maybe B.ByteString
   ,votes        :: Integer
   ,impact       :: Integer
   } deriving (Show)

data Person = Person {
    personId :: Integer
   ,name     :: B.ByteString
   ,score    :: Integer
   ,hits     :: Integer
   ,events   :: M.Map B.ByteString (MVar ScoreInc)
   }

instance Show Person where
    show (Person pid n s i _) = "Person { " ++ (intercalate ", " [show pid, show n, show s, show i]) ++ "}"

data Context = Context {
    contextId   :: Integer
   ,contextName :: B.ByteString
   ,description :: B.ByteString
   } deriving (Show)

data ScoreInc = ScoreInc {
    user     :: Integer
   ,activity :: Integer
   ,delta    :: Integer
   ,initTime :: UTCTime
   } deriving (Show)

data BroState = BroState {
    broconn         :: Connection
   ,mkUserS         :: Statement
   ,mkPersonS       :: Statement
   ,mkContextS      :: Statement
   ,mkScoreIncS     :: Statement
   ,updateUserS     :: Statement
   ,updatePersonS   :: Statement
   ,checkPasswordS  :: Statement
   ,updatePasswordS :: Statement
   ,brousers        :: MVar (M.Map B.ByteString (MVar User))
   ,brocontexts     :: MVar (M.Map B.ByteString (MVar (Context, M.Map B.ByteString (MVar Person))))
   }

-- These functions are for testing purposes only:

pureBro :: (BroState -> MVar a) -> (a -> b) -> (BroState -> IO b)
pureBro = flip ((.) . fmap) . (readMVar .)

numUsers = pureBro brousers M.size
lstUsers = (mapM (liftM show . readMVar) =<<) . pureBro brousers (map snd . M.toList)

numContexts = pureBro brocontexts M.size
lstContexts = (mapM (liftM fst . readMVar) =<<) . pureBro brocontexts (map snd . M.toList)

numPeople = liftM length . lstPeople
lstPeople = (mapM readMVar =<<) . (liftM concat . mapM (liftM ((map snd . M.toList) . snd) . readMVar) =<<) . pureBro brocontexts (map snd . M.toList)
