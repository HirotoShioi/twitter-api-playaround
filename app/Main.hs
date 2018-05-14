{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           RIO

import qualified Data.ByteString.Char8  as C8
import           Network.HTTP.Conduit
import           Say
import           Web.Authenticate.OAuth

import           Types

serverName :: String
serverName = "api.twitter.com"

authFile :: FilePath
authFile = "./secrets/auth"

credentialFile :: FilePath
credentialFile = "./secrets/credentials"

data Config = Config
    { cfgOAuth      :: !OAuth
    , cfgCredential :: !Credential
    , cfgManager    :: !Manager
    }

setOauth :: String -> ByteString -> ByteString -> OAuth -- OAuth
setOauth name key secret = newOAuth
    { oauthServerName     = name
    , oauthConsumerKey    = key
    , oauthConsumerSecret = secret
    }

data ResultType =
      Mixed
    | Recent
    | Popular

renderResultType :: ResultType -> String
renderResultType Mixed   = "mixed"
renderResultType Recent  = "recent"
renderResultType Popular = "popular"

searchUrl :: String -> ResultType -> Int -> String
searchUrl hashtag t counts=
    "https://api.twitter.com/1.1/search/tweets.json?q=%23"
        <> hashtag
        <> "&result_type=" <> renderResultType t
        <> "&count=" <> show counts

timeline :: Config -> String -> ResultType -> Int -> IO [Tweet]
timeline Config{..} query result count = do
    req <- parseRequest $ searchUrl query result count
    signedreq <- signOAuth cfgOAuth cfgCredential req
    res <- httpLbs signedreq cfgManager
    let eitherRes = decodeTweets $ responseBody res
    case eitherRes of
      Left err -> throwString err
      Right ts -> return ts

main :: IO ()
main = do
    auths <- readFileBinary authFile
    credential <- readFileBinary credentialFile
    manager <- newManager tlsManagerSettings
    let (consumerKey: consumerSecret: _) = C8.lines auths
        (token: tokenSecret: _) = C8.lines credential
        oauth = setOauth serverName consumerKey consumerSecret
        cred = newCredential token tokenSecret
        config = Config oauth cred manager
    twts <- timeline config "ねむい" Mixed 100
    say $ "Number of tweets:   " <> tshow (length twts)
    mapM_ (\Tweet{..} -> say $ "**" <> tUserName <> "**\n" <>  tText <> "\n") twts
