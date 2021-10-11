{-# LANGUAGE BangPatterns #-}

import Control.Concurrent.MVar
import Control.Exception
import Prelude hiding (catch) -- use Control.Excpetion's version

-- modifyMVar_strict :: MVar a -> (a -> IO a) -> IO ()
-- modifyMVar_strict m io = block $ do
--     a <- takeMVar m
--     !b <- unblock (io a) `catch` \e ->
--         putMVar m a >> throw e
--     putMVar m b


modifyMVar_mask :: MVar a -> (a -> IO a) -> IO ()
modifyMVar_mask m io = mask $ \restore -> do
    a <- takeMVar m
    !b <- (restore (io a)) `catch` \e ->
        putMVar m a >> throwIO (e :: SomeException)
    putMVar m b
