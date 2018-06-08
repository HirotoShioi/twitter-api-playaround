{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import RIO

import Control.Lens ((.~))
import Test.QuickCheck
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Monadic as Q
import Data.Text.Arbitrary()

import Types
import Lib
import Functions

withStubLayer :: ApiLayer App -> PropertyM IO Config
withStubLayer stubbedLayer = do
    config <- run defaultConfig
    let config' = config & #apiLayer .~ stubbedLayer
    return config'

getZeroTweets :: String -> PropertyM IO Int
getZeroTweets keyword = do
  let stubbedApiLayer :: ApiLayer App
      stubbedApiLayer =
        emptyApiLayer { alFetchHashtagTweets = \_ _ _-> pure [] }

  stubbedConfig <- withStubLayer stubbedApiLayer

  let appExecution :: IO Int
      appExecution = runApp (getTweetCount keyword Mixed) stubbedConfig
  run appExecution

main :: IO ()
main = hspec $
  describe "Twitter api" $ do
    describe "CountTweets" $ modifyMaxSuccess (const 200) $ do
      it "Should return 0 when no matches were found" $
        forAll arbitrary $ \(keyword :: String) ->
            monadicIO $ do
              tweetResponse <- getZeroTweets keyword
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
    
    describe "SearchTweets" $ modifyMaxSuccess (const 200) $ do
      it "Should return list of tweets" $
        forAll (listOf1 arbitrary) $ \(tweets :: [Tweet]) ->
          forAll arbitrary $ \(keyword :: String) ->
            forAll (choose (0,100)) $ \num ->
              monadicIO $ do

                let stubbedApiLayer :: ApiLayer App
                    stubbedApiLayer =
                      emptyApiLayer { alFetchHashtagTweets = \_ _ _ -> pure tweets }

                stubbedConfig <- withStubLayer stubbedApiLayer

                let appExecution :: IO [Tweet]
                    appExecution = runApp (getSearchTweets keyword Mixed num) stubbedConfig
                tweetResponse <- run appExecution

                Q.assert $ not $ null tweetResponse
        
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