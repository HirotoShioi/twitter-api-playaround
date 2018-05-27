{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}

module Lib where

import           RIO

import           Network.HTTP.Conduit
import           Web.Authenticate.OAuth
import           Types
import           Data.Aeson

data ResultType =
      Mixed
    | Recent
    | Popular

data Config = Config
    { cfgOAuth      :: !OAuth
    , cfgCredential :: !Credential
    , cfgManager    :: !Manager
    , cfgAPILayer   :: !(ApiLayer App)
    }

newtype App a = App (ReaderT Config IO a)
    deriving ( Functor
             , Applicative
             , Monad
             , MonadReader Config
             , MonadIO
             , MonadThrow
             )

runApp :: App a -> Config -> IO a
runApp (App a) = runReaderT a

data ApiLayer m = ApiLayer
    { alFetchTweets :: String -> ResultType -> Int -> m [Tweet]
    , alFetchUserTweets :: m [Tweet]
    }

basicApiLayer :: (MonadReader Config m, MonadIO m, MonadThrow m) => ApiLayer m
basicApiLayer = ApiLayer
    { alFetchTweets = fetchTweets
    , alFetchUserTweets = fetchUserTweets
    }

askAPILayer :: forall m a. (MonadReader Config m) => (ApiLayer App -> a) -> m a
askAPILayer getter = do
    Config{..} <- ask
    pure $ getter cfgAPILayer

searchUrl :: String -> ResultType -> Int -> String
searchUrl hashtag t counts=
    "https://api.twitter.com/1.1/search/tweets.json?q=%23"
        <> hashtag
        <> "&result_type=" <> renderResultType t
        <> "&count=" <> show counts

renderResultType :: ResultType -> String
renderResultType Mixed   = "mixed"
renderResultType Recent  = "recent"
renderResultType Popular = "popular"

fetchTweets :: (MonadReader Config m, MonadIO m, MonadThrow m) 
            => String
            -> ResultType
            -> Int
            -> m [Tweet]
fetchTweets query result count = do
    Config{..} <- ask
    req <- parseRequest $ searchUrl query result count
    signedreq <- signOAuth cfgOAuth cfgCredential req
    res <- responseBody <$> httpLbs signedreq cfgManager
    let eitherRes = decodeTweets res
    case eitherRes of
      Left err -> throwString err
      Right ts -> return ts

userTimelineUrl :: String
userTimelineUrl = "https://api.twitter.com/1.1/statuses/user_timeline.json"

fetchUserTweets :: (MonadReader Config m, MonadIO m, MonadThrow m)
              => m [Tweet]
fetchUserTweets = do
    Config{..} <- ask
    req <- parseRequest userTimelineUrl
    signedreq <- signOAuth cfgOAuth cfgCredential req
    res <- responseBody <$> httpLbs signedreq cfgManager
    let eitherRes = eitherDecode res
    case eitherRes of
      Left err -> throwString err
      Right ts -> return ts