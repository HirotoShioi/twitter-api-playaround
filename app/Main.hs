{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE OverloadedLabels  #-}

module Main where

import           RIO

import           Data.Extensible
import qualified Data.ByteString.Char8  as C8
import           Network.HTTP.Conduit
import           Say
import           Web.Authenticate.OAuth

import           CLI                    (CLI(..), getCliArgs)
import           Lib                    (App, Config, ResultType(..),
                                         askAPILayer, basicApiLayer, runApp)

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
    fetchSearchTweets <- askAPILayer (^. #fetchHashtagTweets)
    tws <- fetchSearchTweets query result count
    say $ "Number of tweets:   " <> tshow (length tws)
    mapM_ (\t -> do
        let userName = t ^. #userName
        let twt   = t ^. #tweet
        say $ "**" <> userName <> "**\n" <>  twt <> "\n") tws

userTweets :: App ()
userTweets = do
    fetchUserTweets <- askAPILayer (^. #fetchUserTweets)
    tws <- fetchUserTweets
    forM_ tws (\t -> say (t ^. #tweet))

countTweets :: String -> ResultType -> App ()
countTweets query result = do
    fetchSearchTweets <- askAPILayer (^. #fetchHashtagTweets)
    tweets <- fetchSearchTweets query result 100
    say $ "Fetched " <> tshow (length tweets) <> " tweets!"

main :: IO ()
main = do
    auths      <- readFileBinary authFile
    credential <- readFileBinary credentialFile
    manager    <- newManager tlsManagerSettings
    let (consumerKey: consumerSecret: _) = C8.lines auths
    let (token: tokenSecret: _) = C8.lines credential
    let oauth   = setOauth serverName consumerKey consumerSecret
    let cred    = newCredential token tokenSecret
    let config :: Config
        config  = #oauth      @= oauth
               <: #credential @= cred
               <: #manager    @= manager
               <: #apiLayer   @= basicApiLayer
               <: nil
    args <- getCliArgs
    case args of
        (SearchTweets keyword) -> runApp (sortTweets keyword Mixed 100) config
        (CountTweets keyword)  -> runApp (countTweets keyword Mixed) config
        UserTimeline           -> runApp userTweets config
