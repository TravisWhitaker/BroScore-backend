{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment

import Web.Scotty

import BroScore.DB
import BroScore.WebHandlers

bro bs = do
    post "/mkuser" $ mkUserHandler bs
    post "/mkperson" $ mkPersonHandler bs
    post "/mkcontext" $ mkContextHandler bs
    get  "/auth" $ authUserHandler bs
    post "/newpassword" $ updatePasswordHandler bs
    get  "/contexts" $ getContextsHandler bs
    get  "/people" $ getPeopleHandler bs
    get  "/person" $ getPersonHandler bs
    get  "/user" $ getUserHandler bs
    post "/vote" $ voteHandler bs

main :: IO ()
main = do
    [pw] <- getArgs
    bs <- initBro pw
    scotty 10000 $ bro bs
