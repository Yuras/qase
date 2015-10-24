{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

{- | Write-biased multi-reader lock

 https://en.wikipedia.org/wiki/Readers%E2%80%93writer_lock
-}

module RWLock
( RWLock
, new
, rlock
, runlock
, wlock
, wunlock
, AlreadyUnlocked(..)
)
where

import Data.Typeable
import Control.Monad
import Control.Exception (Exception)
import qualified Control.Exception as Exception
import Control.Concurrent.STM

data WriterState
  = NoWriter
  | OneWriter
  | WaitingWriter

-- | Read-write lock
data RWLock = RWLock
  { readersVar :: TVar Int
  , writerVar :: TVar WriterState
  }

-- | Create new lock
new :: IO RWLock
new = do
  readersVar <- newTVarIO 0
  writerVar <- newTVarIO NoWriter
  return RWLock{..}

-- | Lock for read
rlock :: RWLock -> IO ()
rlock RWLock{..} = atomically $ do
  writer <- readTVar writerVar
  case writer of
    NoWriter ->
      modifyTVar readersVar succ
    _ -> retry

-- | Unlock for read
runlock :: RWLock -> IO ()
runlock RWLock{..} = atomically $ do
  n <- readTVar readersVar
  unless (n > 0) $
    throwSTM AlreadyUnlocked
  writeTVar readersVar (pred n)

-- | Lock for write
wlock :: RWLock -> IO ()
wlock lock = do
  Exception.bracketOnError
    (requestLock lock)
    (const $ Exception.uninterruptibleMask_ $ unrequestLock lock)
  $ const $ do
    waitLock lock

-- | Unlock for write
wunlock :: RWLock -> IO ()
wunlock RWLock{..} = atomically $ do
  writer <- readTVar writerVar
  case writer of
    OneWriter ->
      writeTVar writerVar NoWriter
    _ -> throwSTM AlreadyUnlocked

requestLock :: RWLock -> IO ()
requestLock RWLock{..} = atomically $ do
  writer <- readTVar writerVar
  case writer of
    NoWriter ->
      writeTVar writerVar WaitingWriter
    _ -> retry

unrequestLock :: RWLock -> IO ()
unrequestLock RWLock{..} = atomically $ do
  writeTVar writerVar NoWriter

waitLock :: RWLock -> IO ()
waitLock RWLock{..} = atomically $ do
  n <- readTVar readersVar
  unless (n == 0)
    retry
  writeTVar writerVar OneWriter

-- | Unlock requested without prior lock
data AlreadyUnlocked = AlreadyUnlocked
  deriving (Show, Eq, Typeable)

instance Exception AlreadyUnlocked
