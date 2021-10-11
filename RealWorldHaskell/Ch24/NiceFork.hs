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
import Control.Monad (join)
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
getStatus (Mgr mgr) tid = 
    modifyMVar mgr $ \m ->
        case M.lookup tid m of
          Nothing -> return (m, Nothing)
          Just st -> tryTakeMVar st >>= \mst -> case mst of
                        Nothing -> return (m, Just Running)
                        Just sth -> return (M.delete tid m, Just sth)
    -- Here we are seeing if the tid corresponds to anything in our bookkeeping.
    -- If it does, then we will safely grab its contents.
    -- If the contents do not exist, then we say it's "Running"
    -- If the contents _do_ exist, then we stop book keeping on the thread
    -- and return why the thread stopped running.

-- waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
-- waitFor (Mgr mgr) tid = do
    -- maybeDone <- modifyMVar mgr $ \m ->
    --     return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
    --        (Nothing,  _) -> (m, Nothing)
    --        (done, m') -> (m', done)
    -- case maybeDone of
    --   Nothing -> return Nothing
    --   Just st -> Just `fmap` takeMVar st
    --   -- takeMVar will not do anything until there is a value to take

waitAll :: ThreadManager -> IO ()
waitAll (Mgr mgr) = modifyMVar mgr elems >>= mapM_ takeMVar
    where elems m = return (M.empty, M.elems m)
    -- This ones a bit more straight forward. It just iterates through the elements
    -- and grabs them all.

waitFor :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
waitFor (Mgr mgr) tid = 
    join . modifyMVar mgr $ \m ->
        return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
            (Nothing, _) -> (m, return Nothing)
            (Just st, m) -> (m', Just `fmap` takeMVar st)
            -- Ohhh! Okay, this is useful. I need to take note of this. 
            -- We can merge the second actions in the first output because
            -- we are removing a layer of IO with `join`.
