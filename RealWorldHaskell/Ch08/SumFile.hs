main = do
    contents <- getContents
    print (sumFile contents)
        where sumFile = sum . map read . words
        -- words :: String -> [String]
        -- takes a string and splits it on \s I think
