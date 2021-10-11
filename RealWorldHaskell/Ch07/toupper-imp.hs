import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do
        inh <- openFile "input.txt" ReadMode
        -- The convention 'inh' comes from the fact that openFile returns a handler
        -- so 'inh' => inputhandler
        outh <- openFile "output.txt" WriteMode
        mainloop inh outh
        hClose inh
        hClose outh

mainloop :: Handle -> Handle -> IO ()
-- recursive function that checks if we are at the end of the file for
-- our input file, and if we are, then we return nothing; otherwise, we
-- get the line from out input file, and put the uppercase equivalent
-- into the output file.
mainloop inh outh =
    do ineof <- hIsEOF inh
       -- hIsEOF :: Handle -> IO Bool
       if ineof
          then return ()
          else do inpStr <- hGetLine inh
                  -- hGetLine :: Handle -> IO String
                  hPutStrLn outh (map toUpper inpStr)
                  -- hPutStrLn ... -> IO ()
                  mainloop inh outh

