{-# LANGUAGE RankNTypes #-}

module Async
( childOf
, childOfWithUnmask
, child
)
where

import Control.Monad
import Control.Exception (SomeException)
import qualified Control.Exception as Exception
import Control.Concurrent
import Control.Concurrent.Async (Async, asyncWithUnmask)
import qualified Control.Concurrent.Async as Async
import Control.IO.Region (Region, region)
import qualified Control.IO.Region as Region

-- | Run the action concurrently, linking it to the region. See 'linked'.
childOf :: Region -> IO a -> IO (Async a)
childOf r action = childOfWithUnmask r $ \restore -> restore action


-- | Like 'childOf' but the action initially is in masked state
childOfWithUnmask
  :: Region
  -> ((forall b. IO b -> IO b) ->IO a)
  -> IO (Async a)
childOfWithUnmask r action = do
  handle <- Region.alloc_ r
    (asyncWithUnmask action)
    (Exception.uninterruptibleMask_ . Async.cancel)

  void $ Region.alloc_ r
    (link handle)
    (Exception.uninterruptibleMask_ . killThread)

  return handle

  where
  link handle = do
    me <- myThreadId
    forkRepeat $ do
      res <- Async.waitCatch handle
      case res of
        Left exc -> throwTo me exc
        Right _ -> return ()


-- | Run the first action concurrently with the second one.
-- Both actions are linked: the first one will be killed when
-- the second exists; any exception from the first one will
-- be rethrown to the second one.
--
-- @
-- do
--   channel <- newEmptyMVar
--
--   let worker = forever $ do
--         msg <- takeMVar channel
--         print msg
--
--   child worker $ \_ -> do
--     forM [1..100] $ \i ->
--       putMVar channel i
--
--   -- basically equivalent to the next:
--   concurrently worker $
--     forM [1..100] $ \i ->
--       putMVar channel i
-- @
child :: IO a -> (Async a -> IO b) -> IO b
child action rest = region $ \local -> do
  handle <- childOf local action
  rest handle


-- | Fork a thread and repeat the action unless it succeeds
-- or 'Exception.ThreadKilled' is thrown. All expections are ignored.
forkRepeat :: IO () -> IO ThreadId
forkRepeat action =
  Exception.mask $ \restore -> do
    let go = do
          r <- tryAll $
            restore action
              `Exception.catch` \Exception.ThreadKilled
                -> return ()
          case r of
            Left _ -> go
            _ -> return ()
    forkIO go


-- | Catch all exceptions
tryAll :: IO a -> IO (Either SomeException a)
tryAll = Exception.try
