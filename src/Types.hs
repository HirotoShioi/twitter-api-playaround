{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Types
    ( Tweet(..)
    , decodeTweets 
    ) where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as C8
import           RIO

decodeTweets :: C8.ByteString -> Either String [Tweet]
decodeTweets bs =  do
  jsons <- eitherDecode bs
  parseEither tweets jsons

tweets :: Value -> Parser [Tweet]
tweets = withObject "tweets" $ \o -> o .: "statuses"

data Tweet = Tweet
    { tText     :: Text
    , tUserName :: Text
    } deriving Show

instance ToJSON Tweet where
    toJSON (Tweet text user) =
        object [ "text" .= text
               , "user" .= user
               ]

instance FromJSON Tweet where
    parseJSON = withObject "Tweet" $ \o -> do
      tText  <- o .: "text"
      userO <- o .: "user"
      tUserName <- userO .: "name"
      return Tweet {..}
