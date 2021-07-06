import qualified Data.ByteString.Lazy as L
import System.Environment(getArgs)


hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L.take 4 content == elfMagic
    where elfMagic = L.pack [0x7f, 0x45, 0x4c, 0x46]


isElfFile :: FilePath -> IO Bool
isElfFile path = do
    content <- L.readFile path
    return (hasElfMagic content)


checkElfFile :: Maybe FilePath -> IO String
checkElfFile Nothing = return "No file given."
checkElfFile (Just s) = (isElfFile s) >>= return . show


safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (e:es) = Just e


main :: IO ()
main = do
        arguments <- getArgs
        result <- checkElfFile $ safeHead arguments       
        putStrLn result
