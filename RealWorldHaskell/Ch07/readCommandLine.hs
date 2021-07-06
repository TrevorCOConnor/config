import Data.Char(toUpper)
import System.Environment(getArgs)

lstToUpper :: [String] -> [String]
lstToUpper = map (map toUpper)

main :: IO ()
main = do
        arguments <- getArgs
        mapM_ putStrLn $ lstToUpper arguments
