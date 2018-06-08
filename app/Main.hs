{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Main where

import           RIO

import           Control.Lens           ((.~))
import qualified Data.ByteString.Char8  as C8
import           Network.HTTP.Conduit   (newManager, tlsManagerSettings)
import           Say                    (say)
import           Web.Authenticate.OAuth (OAuth (..), newCredential, newOAuth)

import           CLI                    (CLI (..), getCliArgs)
import           Functions
import           Lib                    (defaultConfig, runApp)
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

printTweets :: [Tweet] -> IO ()
printTweets ts = forM_ ts (\t -> do
    let userName = t ^. #userName
    let twt      = t ^. #tweet
    say $ "**" <> userName <> "**\n" <> twt <> "\n")

main :: IO ()
main = do
    auths      <- readFileBinary authFile
    credential <- readFileBinary credentialFile
    manager    <- newManager tlsManagerSettings
    let (consumerKey: consumerSecret: _) = C8.lines auths
    let (token: tokenSecret: _) = C8.lines credential
    let oauth   = setOauth serverName consumerKey consumerSecret
    let cred    = newCredential token tokenSecret
    config <- defaultConfig
    let config' = config & #manager    .~ manager
                         & #oauth      .~ oauth
                         & #credential .~ cred
    args <- getCliArgs
    -- Configuration done, don't need IO below
    case args of
        (SearchTweets keyword) -> do
                                  tweets <- runApp (getSearchTweets keyword Mixed 100) config'
                                  printTweets tweets
        (CountTweets keyword)  -> do
                                  count <- runApp (getTweetCount keyword Mixed) config'
                                  say $ tshow count
        UserTimeline           -> do
                                  tweets <- runApp getUserTweets config'
                                  printTweets tweets
