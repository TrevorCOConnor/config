import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do
        inh <- openFile "input.txt" ReadMode
        -- opens input file as read only
        outh <- openFile "output.txt" WriteMode
        -- opens output file in write mode
        mainloop inh outh
        -- calls recursive function to iterate through lines
        hClose inh
        -- closes input file
        hClose outh
        -- closes output file

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = 
        do ineof <- hIsEOF inh
           -- hIsEOF :: Handle -> IO Bool
           -- checks if the handler is at EOF
           if ineof
              -- if input is at EOF
              then return ()
              -- we're done here
              else do inpStr <- hGetLine inh
                      -- get next line from handler
                      -- hGetLine :: Handler -> IO String
                      hPutStrLn outh (map toUpper inpStr)
                      -- hPutStrLn :: Handler -> String -> IO () 
                      mainloop inh outh
                      -- recursive call
