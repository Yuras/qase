{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

{- | Wait group, aka Asynchronous semaphore

https://en.wikipedia.org/wiki/Asynchronous_semaphore
-}

module WaitGroup
( WaitGroup
, new
, add
, done
, wait
, AlreadyEmpty (..)
)
where

import Data.Typeable
import Control.Monad
import Control.Exception (Exception)
import Control.Concurrent.STM

-- | Wait group
--
-- Semantically it is just an integer,
-- 'add' increases it and 'done' decreases.
-- 'wait' blocks until the integer becomes zero
data WaitGroup = WaitGroup
  { countVar :: TVar Int
  , readyVar :: TVar Bool
  }

-- | New wait group
new :: IO WaitGroup
new = WaitGroup
  <$> newTVarIO 0
  <*> newTVarIO True

-- | One more event to wait
add :: WaitGroup -> IO ()
add WaitGroup{..} = atomically $ do
  n <- readTVar countVar
  unless (n > 0) $
    writeTVar readyVar False
  writeTVar countVar (succ n)


-- | One event occurs
done :: WaitGroup -> IO ()
done WaitGroup{..} = atomically $ do
  n <- readTVar countVar
  unless (n > 0) $
    throwSTM AlreadyEmpty

  let n' = pred n
  writeTVar countVar n'

  when (n' == 0) $
    writeTVar readyVar True


-- | Wait for all events
wait :: WaitGroup -> IO ()
wait WaitGroup{..} = atomically $ do
  ready <- readTVar readyVar
  case ready of
    True -> return ()
    False -> retry

-- | Exception occurs when 'done' is called without 'add'
data AlreadyEmpty = AlreadyEmpty
  deriving (Show, Eq, Typeable)

instance Exception AlreadyEmpty
