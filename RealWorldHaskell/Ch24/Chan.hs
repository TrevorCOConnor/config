import Control.Concurrent
import Control.Concurrent.Chan

chanExample = do
    ch <- newChan
    forkIO $ do
        writeChan ch "hello world!"
        writeChan ch "now i quite"
    readChan ch >>= print
    readChan ch >>= print
    -- we do a similar thing here to streamline the process
