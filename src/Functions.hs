module Functions
    ( getTweetCount
    , getSearchTweets
    , getUserTweets
    ) where

import           RIO

import           Lib (App, askAPILayer, ApiLayer(..))
import           Types (ResultType, Tweet)

getTweetCount :: String -> ResultType -> App Int
getTweetCount query result = do
    fetchSearchTweets <- askAPILayer alFetchHashtagTweets
    tweets <- fetchSearchTweets query result 100
    return $ length tweets

getSearchTweets :: String -> ResultType -> Int  -> App [Tweet]
getSearchTweets query result count = do
    fetchSearchTweets <- askAPILayer alFetchHashtagTweets
    fetchSearchTweets query result count

getUserTweets :: App [Tweet]
getUserTweets = join $ askAPILayer alFetchUserTweets
