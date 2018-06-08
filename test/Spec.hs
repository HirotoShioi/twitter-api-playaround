{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           RIO

import           Control.Lens            ((.~))
import           Data.Text.Arbitrary     ()
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Monadic as Q

import           Functions               (getSearchTweets, getTweetCount, getUserTweets)
import           Lib                     (ApiLayer (..), App, Config,
                                          defaultConfig, emptyApiLayer, runApp)
import           Types                   (ResultType (..), Tweet)

withStubLayer :: ApiLayer App -> PropertyM IO Config
withStubLayer stubbedLayer = do
    config <- run defaultConfig
    let config' = config & #apiLayer .~ stubbedLayer
    return config'

main :: IO ()
main = hspec $
  describe "Twitter api" $ do
    countTweetSpec
    searchTweetSpec
    userTweetSpec

countTweetSpec :: Spec
countTweetSpec = describe "CountTweets" $ modifyMaxSuccess (const 200) $ do
  it "Should return 0 when no matches were found" $
    forAll arbitrary $ \(keyword :: String) ->
        monadicIO $ do
          let stubbedApiLayer :: ApiLayer App
              stubbedApiLayer =
                emptyApiLayer { alFetchHashtagTweets = \_ _ _-> pure [] }
    
          stubbedConfig <- withStubLayer stubbedApiLayer
    
          let appExecution :: IO Int
              appExecution = runApp (getTweetCount keyword Mixed) stubbedConfig
          tweetResponse <- run appExecution
          Q.assert $ tweetResponse == 0
  it "Should return a number of tweets when there's matches" $
    forAll (listOf1 arbitrary) $ \(tweets :: [Tweet]) ->
      forAll arbitrary $ \(keyword :: String) ->
        monadicIO $ do

          let stubbedApiLayer :: ApiLayer App
              stubbedApiLayer =
                emptyApiLayer { alFetchHashtagTweets = \_ _ _ -> pure tweets }

          stubbedConfig <- withStubLayer stubbedApiLayer

          let appExecution :: IO Int
              appExecution = runApp (getTweetCount keyword Mixed) stubbedConfig
          tweetResponse <- run appExecution

          Q.assert $ tweetResponse /= 0

searchTweetSpec :: Spec
searchTweetSpec = describe "SearchTweets" $ modifyMaxSuccess (const 200) $ do
  it "Should return empty list when there's no matching tweets" $
    forAll arbitrary $ \(keyword :: String) ->
      forAll (choose (0,100)) $ \num ->
        monadicIO $ do

          let stubbedApiLayer :: ApiLayer App
              stubbedApiLayer =
                emptyApiLayer { alFetchHashtagTweets = \_ _ _ -> pure [] }

          stubbedConfig <- withStubLayer stubbedApiLayer

          let appExecution :: IO [Tweet]
              appExecution = runApp (getSearchTweets keyword Mixed num) stubbedConfig
          tweetResponse <- run appExecution

          Q.assert $ null tweetResponse
  it "Should return given number of tweets" $
    forAll (choose (0,100)) $ \num ->
      forAll (vectorOf num arbitrary) $ \(tweets :: [Tweet]) ->
        forAll arbitrary $ \(keyword :: String) ->
          monadicIO $ do

            let stubbedApiLayer :: ApiLayer App
                stubbedApiLayer =
                  emptyApiLayer { alFetchHashtagTweets = \_ _ _ -> pure tweets }

            stubbedConfig <- withStubLayer stubbedApiLayer

            let appExecution :: IO [Tweet]
                appExecution = runApp (getSearchTweets keyword Mixed num) stubbedConfig
            tweetResponse <- run appExecution

            Q.assert $ length tweetResponse == num

userTweetSpec :: Spec
userTweetSpec = 
  describe "UserTweets" $ modifyMaxSuccess (const 200) $ do
    it "Should return empty list if the user has no tweets" $
      monadicIO $ do

        let stubbedApiLayer :: ApiLayer App
            stubbedApiLayer =
              emptyApiLayer { alFetchUserTweets = pure [] }

        stubbedConfig <- withStubLayer stubbedApiLayer

        let appExecution :: IO [Tweet]
            appExecution = runApp getUserTweets stubbedConfig

        tweetResponse <- run appExecution

        Q.assert $ null tweetResponse

    it "Should return list of user's tweets" $
      forAll (listOf1 arbitrary) $ \(tweets :: [Tweet]) ->
        monadicIO $ do

          let stubbedApiLayer :: ApiLayer App
              stubbedApiLayer =
                emptyApiLayer { alFetchUserTweets = pure tweets }
          
          stubbedConfig <- withStubLayer stubbedApiLayer

          let appExecution :: IO [Tweet]
              appExecution = runApp getUserTweets stubbedConfig

          tweetResponse <- run appExecution

          Q.assert $ not $ null tweetResponse