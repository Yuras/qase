{-# LANGUAGE OverloadedStrings #-}

module Spec.Aeson
( spec
)
where

import Aeson

import Prelude hiding (or)
import Data.Text (Text)
import qualified Data.List as List
import qualified Data.Vector as Vector
import qualified Data.HashMap.Strict as HashMap
import Data.Aeson
import Data.Aeson.Types
import Test.Hspec

spec :: Spec
spec = describe "Aeson" $ do
  describe "vector" $ do
    it "should parse json array" $ do
      let p = vector int
          v = toJSON [1, 2 :: Int]
      parseMaybe p v `shouldBe` Just (Vector.fromList [1, 2])

  describe "list" $ do
    it "should parse json array" $ do
      let p = list int
          v = toJSON [1, 2 :: Int]
      parseMaybe p v `shouldBe` Just [1, 2]

  describe "key" $ do
    it "should parse the specified key of an object" $ do
      let p = key "a" int
          v = object ["a" .= (42 :: Int)]
      parseMaybe p v `shouldBe` Just 42

    it "should fail when key is not found" $ do
      let p = key "a" int
          v = object ["b" .= (42 :: Int)]
      parseMaybe p v `shouldBe` Nothing

  describe "opt" $ do
    it "should parse the specified key of an object" $ do
      let p = opt "a" int
          v = object ["a" .= (42 :: Int)]
      parseMaybe p v `shouldBe` Just (Just 42)

    it "should return Nothing when the key doesn't exist" $ do
      let p = opt "a" int
          v = object ["b" .= (42 :: Int)]
      parseMaybe p v `shouldBe` Just Nothing

  describe "def" $ do
    it "should convert optional parser to total one" $ do
      let p v_ = opt "a" int v_ `def` 0
          v = object ["b" .= (42 :: Int)]
      parseMaybe p v `shouldBe` Just 0

  describe "or" $ do
    it "should convert optional parser to total one" $ do
      let p = opt "a" int `or` 0
          v = object ["b" .= (42 :: Int)]
      parseMaybe p v `shouldBe` Just 0

  describe "int" $ do
    it "should parse int" $ do
      let p = int
          v = toJSON (42 :: Int)
      parseMaybe p v `shouldBe` Just 42

  describe "double" $ do
    it "should parse double" $ do
      let p = double
          v = toJSON (42.24 :: Double)
      parseMaybe p v `shouldBe` Just 42.24

  describe "text" $ do
    it "should parse text" $ do
      let p = text
          v = toJSON ("hello" :: Text)
      parseMaybe p v `shouldBe` Just "hello"

  describe "lookupList" $ do
    it "should parse an object" $ do
      let p = lookupList int
          v = object ["a" .= (4 :: Int), "b" .= (2 :: Int)]
      fmap List.sort (parseMaybe p v)
        `shouldBe` Just (List.sort [("a", 4), ("b", 2)])

  describe "hashMap" $ do
    it "should parse an object" $ do
      let p = hashMap int
          v = object ["a" .= (4 :: Int), "b" .= (2 :: Int)]
      parseMaybe p v
        `shouldBe` Just (HashMap.fromList [("a", 4), ("b", 2)])
