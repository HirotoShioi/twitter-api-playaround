{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeOperators              #-}

module Lib where

import           RIO

import           Data.Extensible
import           Network.HTTP.Conduit   (Manager, httpLbs, parseRequest,
                                         responseBody)
import           Web.Authenticate.OAuth (Credential, OAuth, signOAuth)

import           Types                  (Tweet, decodeTweets, decodeUserTweets)

data ResultType =
      Mixed
    | Recent
    | Popular

renderResultType :: ResultType -> String
renderResultType Mixed   = "mixed"
renderResultType Recent  = "recent"
renderResultType Popular = "popular"

type Config = Record
    '[ "oauth"      >: OAuth
     , "credential" >: Credential
     , "manager"    >: Manager
     , "apiLayer"   >: ApiLayer App
     ]

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

type ApiLayer m = Record
    '[ "fetchHashtagTweets" >: (String -> ResultType -> Int -> m [Tweet])
     , "fetchUserTweets"    >: m [Tweet]
     ]

basicApiLayer :: (MonadReader Config m, MonadIO m, MonadThrow m) => ApiLayer m
basicApiLayer = #fetchHashtagTweets @= fetchHashtagTweets
             <: #fetchUserTweets    @= fetchUserTweets
             <: nil

askAPILayer :: forall m a. (MonadReader Config m) => (ApiLayer App -> a) -> m a
askAPILayer getter = do
    cfg <- ask
    pure $ getter (cfg ^. #apiLayer)

fetchTweets ::  (MonadReader Config m, MonadIO m, MonadThrow m)
                => String
                -> m LByteString
fetchTweets url = do
    cfg <- ask
    let oauth      = cfg ^. #oauth
    let credential = cfg ^. #credential
    let manager    = cfg ^. #manager
    req <- parseRequest url
    signedreq <- signOAuth oauth credential req
    responseBody <$> httpLbs signedreq manager

searchUrl :: String -> ResultType -> Int -> String
searchUrl hashtag t counts=
    "https://api.twitter.com/1.1/search/tweets.json?q=%23"
        <> hashtag
        <> "&result_type=" <> renderResultType t
        <> "&count=" <> show counts

fetchHashtagTweets :: (MonadReader Config m, MonadIO m, MonadThrow m)
            => String
            -> ResultType
            -> Int
            -> m [Tweet]
fetchHashtagTweets query result count = do
    res <- fetchTweets (searchUrl query result count)
    let eitherRes = decodeTweets res
    case eitherRes of
      Left err -> throwString err
      Right ts -> return ts

userTimelineUrl :: String
userTimelineUrl = "https://api.twitter.com/1.1/statuses/user_timeline.json"

fetchUserTweets :: (MonadReader Config m, MonadIO m, MonadThrow m)
              => m [Tweet]
fetchUserTweets = do
    res <- fetchTweets userTimelineUrl
    let eitherRes = decodeUserTweets res
    case eitherRes of
      Left err -> throwString err
      Right ts -> return ts
