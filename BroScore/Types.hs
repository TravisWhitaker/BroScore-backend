module BroScore.Types where

import Control.Concurrent.MVar

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map                   as M
import Data.Time.LocalTime

import Database.HDBC

data User = User {
    id             :: Integer
   ,userName       :: B.ByteString
   ,ephemeralKey   :: B.ByteString
   ,activity       :: Integer
   ,effect         :: Integer
   }

data Person = Person {
    id       :: Integer
   ,name     :: B.ByteString
   ,score    :: Integer
   ,activity :: Integer
   ,events   :: M.Map B.ByteString ScoreInc
   }

data Context = Context {
    id          :: Integer
   ,name        :: B.ByteString
   ,description :: B.ByteString
   }

data ScoreInc = ScoreInc {
    user     :: User
   ,activity :: Integer
   ,effect   :: Integer
   ,initTime :: LocalTime
   }

data BroState = BroState {
    broconn     :: IConnection conn => conn
   ,mkUser      :: Statement
   ,mkPerson    :: Statement
   ,mkContext   :: Statement
   ,mkScoreInc  :: Statement
   ,brousers    :: M.Map B.ByteString User
   ,brocontexts :: M.Map B.ByteString (Context, M.Map B.ByteString Person)
   }

newtype Bro = Bro {unBro :: MVar BroState}
