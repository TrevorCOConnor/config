module NiceFork
    ( ThreadManager
    , newManager
    , forkManaged
    , getStatus
    , waitFor
    , waitAll
    ) where

import Control.Concurrent
import Control.Exception (Exception, try)
import qualified Data.Map as M


newtype ThreadManager = 
    Mgr (MVar (M.Map ThreadId (MVar ThreadStatus)))
        deriving (Eq)
    -- This makes a map type where the keys are thread ids,
    -- and the values are the thread statuses

data ThreadStatus = Running
                  | Finished            -- terminated normally
                  | ThrewException      -- killed by uncaught exception
                    deriving (Eq, Show)


newManager :: IO ThreadManager
newManager = Mgr `fmap` newMVar M.empty
    -- fmap needed to put result in IO

forkManaged :: ThreadManager -> IO () -> IO ThreadId
forkManaged (Mgr mgr) body =
    modifyMVar mgr $ \m -> do
        state <- newEmptyMVar
        tid <- forkIO $ do
            result <- try body
            putMVar state (either Threw (const Finished) result)
        return (M.insert tid state m, tid)

getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
getStatus = undefined

waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
waitFor = undefined

waitAll :: ThreadManager -> IO ()
waitAll = undefined
