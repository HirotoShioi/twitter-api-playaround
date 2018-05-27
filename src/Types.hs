{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Types
    ( Tweet
    , decodeTweets
    , decodeUserTweets
    ) where

import           RIO

import           Data.Aeson                 (Value, eitherDecode)
import           Data.Aeson.Types           (Parser, parseEither, withObject,
                                             (.:))
import qualified Data.ByteString.Lazy.Char8 as C8
import           Data.Extensible

decodeTweets :: C8.ByteString -> Either String [Tweet]
decodeTweets bs =  do
  jsons <- eitherDecode bs
  parseEither tweets jsons

tweets :: Value -> Parser [Tweet]
tweets = withObject "tweets" $ \o -> do
    tws <- o .: "statuses"
    mapM decodeTweet tws


decodeUserTweets :: C8.ByteString -> Either String [Tweet]
decodeUserTweets bs =  do
    jsons <- eitherDecode bs
    parseEither (mapM decodeTweet) jsons

type Tweet = Record
    '[ "tweet"    >: Text
     , "userName" >: Text
     ]

decodeTweet :: Value -> Parser Tweet
decodeTweet = withObject "Tweet" $ \o -> do
    text  <- o .: "text"
    userO <- o .: "user"
    name <- userO .: "name"
    return $ #tweet     @= text
          <: #userName  @= name
          <: nil
