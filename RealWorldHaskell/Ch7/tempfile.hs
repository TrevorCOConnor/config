import System.IO
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO.Error (catchIOError)
import Control.Exception (finally)

main :: IO ()
main = withTempFile "mytemp.txt" myAction

myAction :: FilePath -> Handle -> IO ()
myAction tempname temph =
    do
        putStrLn "Welcome to tempfile.hs"
        putStrLn $ "I have a temporary file at " ++ tempname
        
        pos <- hTell temph
        putStrLn $ "My initial position is " ++ show pos

        let tempdata = show [1..10]
        putStrLn $ "Writing one line containing " ++
                    show (length tempdata) ++ " bytes: " ++
                    tempdata

        hPutStrLn temph tempdata

        pos <- hTell temph
        putStrLn $ "After writing, my new position is " ++ show pos

        putStrLn $ "The file content is: "
        hSeek temph AbsoluteSeek 0

        
        -- hGetContents performs a lazy read of the entire file
        c <- hGetContents temph

        putStrLn c

        putStrLn $ "Which could be expressed as this Haskel literal:"
        print c


withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func = 
    do 
        tempdir <- catchIOError (getTemporaryDirectory) (\_ -> return ".")
        -- this returns "." (the current directory) if the system cannot create a 
        -- temporary directory
        (tempfile, temph) <- openTempFile tempdir pattern
        
        finally (func tempfile temph)
                (do hClose temph
                    removeFile tempfile)
