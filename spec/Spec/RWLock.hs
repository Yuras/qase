
module Spec.RWLock
( spec
)
where

import qualified RWLock

import System.Timeout
import Test.Hspec

spec :: Spec
spec = describe "RWLock" $ do
  describe "rlock" $ do
    it "should acquire lock for read" $ do
      l <- RWLock.new
      RWLock.rlock l
      RWLock.runlock l

    it "should acquire lock if there are more readers already" $ do
      l <- RWLock.new
      RWLock.rlock l
      RWLock.rlock l
      RWLock.runlock l
      RWLock.runlock l

    it "should block when there is a writer" $ do
      l <- RWLock.new
      RWLock.wlock l
      res <- timeout 1000 (RWLock.rlock l)
      res `shouldBe` Nothing
      RWLock.wunlock l
      RWLock.rlock l

  describe "wlock" $ do
    it "should acquire lock for write" $ do
      l <- RWLock.new
      RWLock.wlock l
      RWLock.wunlock l

    it "should block when there is other writer" $ do
      l <- RWLock.new
      RWLock.wlock l
      res <- timeout 1000 (RWLock.wlock l)
      res `shouldBe` Nothing
      RWLock.wunlock l
      RWLock.wlock l

    it "should block when there is other reader" $ do
      l <- RWLock.new
      RWLock.rlock l
      res <- timeout 1000 (RWLock.wlock l)
      res `shouldBe` Nothing
      RWLock.runlock l
      RWLock.wlock l

  describe "runlock" $ do
    it "should throw when called without rlock" $ do
      l <- RWLock.new
      RWLock.runlock l
        `shouldThrow` (== RWLock.AlreadyUnlocked)
      RWLock.rlock l

  describe "wunlock" $ do
    it "should throw when called without wlock" $ do
      l <- RWLock.new
      RWLock.wunlock l
        `shouldThrow` (== RWLock.AlreadyUnlocked)
      RWLock.wlock l
