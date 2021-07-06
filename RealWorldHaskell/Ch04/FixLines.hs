module FixLines where

import SplitLines

fixLines :: String -> String
fixLines input = unlines (splitLines input)
