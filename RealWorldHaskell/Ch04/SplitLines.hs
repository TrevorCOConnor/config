module SplitLines where

splitLines :: String -> [String]
splitLines [] = []
splitLines cs = pre : (splitLines rest)
    where (pre, suf) = break isLineTerminator cs
          rest = case suf of
                ('\r':'\n':rest) -> rest
                ('\r':rest)      -> rest
                ('\n':rest)      -> rest
                _                -> []

          isLineTerminator c = c `elem` "\r\n"
