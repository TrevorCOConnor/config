import Data.Char(toUpper)

main = interact (map toUpper . (++) "Your data, in uppercase, is:\n\n")
-- interact :: (String -> String) -> IO ()
-- map toUpper :: String -> String
-- (++) :: String -> String -> String
-- map toUpper . (++) "..." :: String -> String
-- 
-- But the string fed to (map toUpper) will have our text in it.
