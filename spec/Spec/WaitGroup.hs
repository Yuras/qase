
module Spec.WaitGroup
( spec
)
where

import qualified WaitGroup

import Control.Monad
import Control.Concurrent
import Test.Hspec

spec :: Spec
spec = describe "WaitGroup" $ do
  it "should wait for exactly the number of 'add's" $ do
    wg <- WaitGroup.new
    WaitGroup.add wg
    WaitGroup.add wg
    WaitGroup.add wg
    var <- newEmptyMVar

    void $ forkIO $ do
      WaitGroup.done wg
      WaitGroup.done wg
      putMVar var ()
      threadDelay 1000
      putMVar var ()
      WaitGroup.done wg

    takeMVar var
    WaitGroup.wait wg
    v <- tryTakeMVar var
    v `shouldBe` Just ()
      
  it "should throw when 'done' called without 'add'" $ do
    wg <- WaitGroup.new
    WaitGroup.add wg
    WaitGroup.done wg
    WaitGroup.done wg
      `shouldThrow` (== WaitGroup.AlreadyEmpty)
