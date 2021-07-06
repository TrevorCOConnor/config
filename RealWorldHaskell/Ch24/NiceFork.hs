module NiceFork
    ( ThreadManager
    , newManager
    , forkManager
    , getStatus
    , waitFor
    , waitAll
    ) where

import Control.Concurrent
import Control.Exception (handle, try)
import qualified Data.Map as M

data ThreadStatus = Running
                  | Finished        -- terminated normally
                  | Thew Exception  -- killed by uncaught exception
                    deriving (Eq, Show)

newManager :: IO ThreadManager

forkManager :: ThreadManager -> IO () -> IO ThreadId

getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)

waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)

waitAll :: ThreadManager -> IO ()


newtype ThreadManager = 
    Mgr (MVar (M.map ThreadId (MVar ThreadStatus)))
    deriving (Eq)

newManager = Mgr `fmap` newMVar M.empty
