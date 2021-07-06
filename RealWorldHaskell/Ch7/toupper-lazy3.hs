import Data.Char(toUpper)

main = do
        inpStr <- readFile "mobyDick.txt"
        writeFile "output.txt" (map toUpper inpStr)
