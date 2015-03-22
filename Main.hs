{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment

import Web.Scotty

import BroScore.DB
import BroScore.WebHandlers

bro bs = do
    post "/mkuser" $ mkUserHandler bs
    post "/mkperson" $ authAction bs mkPersonHandler
    post "/mkcontext" $ authAction bs mkContextHandler
    get  "/auth" $ authUserHandler bs
    post "/newpassword" $ updatePasswordHandler bs
    get  "/contexts" $ getContextsHandler bs
    get  "/people" $ getPeopleHandler bs
    get  "/person" $ getPersonHandler bs
    get  "/user" $ getUserHandler bs
    post "/vote" $ authAction bs voteHandler

main :: IO ()
main = do
    [pw] <- getArgs
    bs <- initBro pw
    scotty 10000 $ bro bs
