import Control.Concurrent

notQuiteRight :: IO ()
notQuiteRight = do
    mv <- newEmptyMVar
    forkIO $ expensiveComputation mv
    result <- takeMVar mv
    print result


expensiveComputation :: MVar [Char] -> IO ()
expensiveComputation mv = do
    let a = "this is "
        b = "not really "
        c = "all that expensive"
    putMVar mv (a ++ b ++ c)
