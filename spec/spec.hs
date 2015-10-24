
module Main
( main
)
where

import qualified Spec.Async
import qualified Spec.WaitGroup
import qualified Spec.RWLock

import Test.Hspec

main :: IO ()
main = hspec $ do
  Spec.Async.spec
  Spec.WaitGroup.spec
  Spec.RWLock.spec
