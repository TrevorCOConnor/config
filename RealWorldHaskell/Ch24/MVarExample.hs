import Control.Concurrent

communicate = do
    m <- newEmptyMVar
    forkIO $ do
        v <- takeMVar m
        putStrLn ("received " ++ show v)
    putStrLn "sending"
    putMVar m "wake up!"


-- We start the thread on line 7 with forkIO, but it sleeps because the mvar is empty.
-- Once we `putMVar m "wake up!"` we put something in that thread that wakes it up,
-- and it run subsequently.
