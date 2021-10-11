import System.IO
import System.Directory(getTemporaryDirectory, removeFile)
import System.IO.Error(catchIOError)
import Control.Exception(finally)

main :: IO ()
main = withTempFile "mytemp.txt" myAction
-- withTempFile and myAction tbd

myAction :: FilePath -> Handle -> IO ()
myAction tempname temph =
    do 
        putStrLn "Welcome to tempfile.hs"
        putStrLn $ "I have a temporary file at " ++ tempname

        pos <- hTell temph
        putStrLn $ "My initial position is " ++ show pos

        let tempdata = show [1..10]
        putStrLn $ "Writing one line containing " ++
                   show (length tempdata) ++ " bytes:" ++
                   tempdata
        hPutStrLn temph tempdata

        pos <- hTell temph
        putStrLn $ "After writing, my new posiiton is " ++ show pos

        putStrLn $ "The file content is: "
        hSeek temph AbsoluteSeek 0

        c <- hGetContents temph
        -- Note that here, we are doing a lazy read of the entire temp file

        putStrLn c

        putStrLn "Which could be expressed as this Haskell literal:"
        print c

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func = 
    do tempdir <- catchIOError (getTemporaryDirectory) (\_ -> return ".")
                  -- catchIOError :: IO a -> (IOError -> IO a) -> IO a
                  -- a :: FilePath which is set by getTemporaryDirectory :: IO FilePath
                  -- the lambda returns the filepath of the current directory.
                  -- This also shows that FilePath is synonymous with String.
       (tempfile, temph) <- openTempFile tempdir pattern
       -- openTempFile :: FilePath -> String -> IO (FilePath, Handle)
       -- Does this also mean that () is just an empty tuple???
       finally (func tempfile temph)
               (do hClose temph
                   removeFile tempfile)
       -- finally :: IO a -> IO b -> IO a
