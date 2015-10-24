{-# LANGUAGE DeriveDataTypeable #-}

module Spec.Async
( spec
)
where

import qualified Async

import Data.Typeable
import Control.Exception (Exception, throwIO, finally)
import Control.Concurrent
import Test.Hspec

spec :: Spec
spec = describe "Async" $ do
  childOfSpec

childOfSpec :: Spec
childOfSpec = describe "childOf" $ do
  it "should run the child" $ do
    var <- newEmptyMVar
    let child = putMVar var ()
    Async.child child $ \_ -> do
      takeMVar var

  it "should abort the child on exit from main" $ do
    var <- newEmptyMVar
    let child =
          (putMVar var () >> threadDelay 10000000)
            `finally` putMVar var ()
    Async.child child $ \_ -> do
      takeMVar var

    takeMVar var

  it "should rethow any exception from the child" $ do
    var <- newEmptyMVar
    let child = do
          putMVar var ()
          throwIO Ex
    (Async.child child $ \_ -> do
      takeMVar var >> takeMVar var
      ) `shouldThrow` (== Ex)

data Ex = Ex
  deriving (Show, Eq, Typeable)

instance Exception Ex
