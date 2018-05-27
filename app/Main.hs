{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           RIO

import qualified Data.ByteString.Char8  as C8
import           Network.HTTP.Conduit
import           Say
import           Web.Authenticate.OAuth

import           Lib                    (App, Config (..), ResultType(..), ApiLayer(..),
                                         askAPILayer, basicApiLayer, runApp)
import           Types

serverName :: String
serverName = "api.twitter.com"

authFile :: FilePath
authFile = "./secrets/auth"

credentialFile :: FilePath
credentialFile = "./secrets/credentials"

setOauth :: String -> ByteString -> ByteString -> OAuth -- OAuth
setOauth name key secret = newOAuth
    { oauthServerName     = name
    , oauthConsumerKey    = key
    , oauthConsumerSecret = secret
    }

sortTweets :: String -> ResultType -> Int  -> App ()
sortTweets query result count = do
    fetchSearchTweets <- askAPILayer alFetchTweets
    tws <- fetchSearchTweets query result count
    say $ "Number of tweets:   " <> tshow (length tws)
    mapM_ (\Tweet{..} -> say $ "**" <> tUserName <> "**\n" <>  tText <> "\n") tws

userTweets :: App ()
userTweets = do
    fetchUserTweets <- askAPILayer alFetchUserTweets
    tws <- fetchUserTweets
    forM_ tws (\Tweet{..} ->
        say tText)

countTweets :: String -> ResultType -> App ()
countTweets query result = do
    fetchSearchTweets <- askAPILayer alFetchTweets
    counts <- length <$> fetchSearchTweets query result 100
    say $ "Fetched " <> tshow counts <> " tweets!"

main :: IO ()
main = do
    auths      <- readFileBinary authFile
    credential <- readFileBinary credentialFile
    manager    <- newManager tlsManagerSettings
    let (consumerKey: consumerSecret: _) = C8.lines auths
    let (token: tokenSecret: _) = C8.lines credential
    let oauth   = setOauth serverName consumerKey consumerSecret
    let cred    = newCredential token tokenSecret
    let keyword = "ポケモン"
    let config  = Config { cfgOAuth      = oauth
                         , cfgCredential = cred
                         , cfgManager    = manager
                         , cfgAPILayer   = basicApiLayer
                         }
    runApp userTweets config
    runApp (sortTweets keyword Mixed 100) config
    runApp (countTweets keyword Mixed) config
