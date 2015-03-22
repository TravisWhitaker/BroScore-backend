{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module BroScore.WebHandlers where

import Control.Monad
import Control.Monad.IO.Class

import Data.Aeson hiding (json, encode, decode)

import Data.ByteString.Lazy.Char8      as B

import Data.ByteString.Base64.URL.Lazy

import Data.Text.Encoding

import Network.HTTP.Types.Status

import Web.Scotty

import BroScore.Types
import BroScore.DB
import BroScore.Query

instance ToJSON B.ByteString where
    toJSON = String . decodeUtf8 . B.toStrict

instance ToJSON Context where
    toJSON (Context cid cn d) = object ["contextId" .= cid
                                       ,"name" .= cn
                                       ,"description" .= d
                                       ]

instance ToJSON Person where
    toJSON (Person pid pn s h _) = object ["personId" .= pid
                                          ,"name" .= pn
                                          ,"score" .= s
                                          ,"hits" .= h
                                          ]

instance ToJSON User where
    toJSON (User uid un k v i) = object ["userId" .= uid
                                        ,"name" .= un
                                        ,"key" .= (fmap encode k)
                                        ,"votes" .= v
                                        ,"impact" .= i
                                        ]

type BroAction = BroState -> ActionM ()

authAction :: BroState -> BroAction -> ActionM ()
authAction bs ba = do
    un <- param "username"
    pw <- param "passowrd"
    liftIO (checkAuth bs un pw) >>= \case Just s -> status s
                                          Nothing -> ba bs

mkUserHandler :: BroAction
mkUserHandler bs = do
    un <- param "username"
    pw <- param "password"
    liftIO (mkUser bs un pw) >>= \case Left s -> status s
                                       Right k -> json $ object ["key" .= (encode k)]

mkPersonHandler :: BroAction
mkPersonHandler bs = do
    cn <- param "context"
    pn <- param "name"
    liftIO (mkPerson bs cn pn) >>= \case Just s -> status s
                                         Nothing -> return ()

mkContextHandler :: BroAction
mkContextHandler bs = do
    cn <- param "context"
    ds <- param "description"
    liftIO (mkContext bs cn ds) >>= \case Just s -> status s
                                          Nothing -> return ()

authUserHandler :: BroAction
authUserHandler bs = do
    un <- param "username"
    pw <- param "password"
    liftIO (authUser bs un pw) >>= \case Left s -> status s
                                         Right k -> json $ object ["key" .= (encode k)]

updatePasswordHandler :: BroAction
updatePasswordHandler bs = do
    un <- param "username"
    k  <- param "key"
    pw <- param "password"
    liftIO (updatePassword bs un (decodeLenient k) pw) >>= \case Left s -> status s
                                                                 Right k -> json $ object ["key" .= (encode k)]

getContextsHandler :: BroAction
getContextsHandler bs = liftIO (getContexts bs) >>= json

getPeopleHandler :: BroAction
getPeopleHandler bs = param "context" >>= liftIO . getPeople bs >>= json

getPersonHandler :: BroAction
getPersonHandler bs = do
    cn <- param "context"
    pn <- param "name"
    liftIO (getPerson bs cn pn) >>= \case Nothing -> status status404
                                          Just p -> json p

getUserHandler :: BroAction
getUserHandler bs = do
    un <- param "username"
    k <- param "key"
    liftIO (getUser bs un (decodeLenient k)) >>= \case Left s -> status s
                                                       Right u -> json u

voteHandler :: BroAction
voteHandler bs = do
    un <- param "username"
    k  <- param "key"
    cn <- param "context"
    pn <- param "name"
    liftIO (vote bs un (decodeLenient k) cn pn 1) >>= \case Just s -> status s
                                                            Nothing -> return ()
