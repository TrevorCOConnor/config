module Switch (switch) where

import XMonad.Core
import XMonad.Util.Run
import Text.Printf
import System.Process

listSinks :: X String
listSinks = runProcessWithInput "pacmd" ["list-sinks"] ""

breakLines :: String -> [String]
breakLines text = transform text []
    where transform []        _   = []
          transform ('\n':ts) str = (reverse str) : transform ts []
          transform (t:[])    str = transform ['\n'] (t:str)
          transform (t:ts)    str = transform ts (t:str) 

getIndices :: [String] -> [String]
getIndices [] = []
getIndices (t:ts) | t' == "  * index:" = t : getIndices ts
                  | t' == "    index:" = t : getIndices ts
                  | otherwise         = getIndices ts
                  where t' = take 10 t

getNextIndex :: [String] -> String
getNextIndex []      = []
getNextIndex indices | indices' == [] = extractNum $ head indices
                     | otherwise      = extractNum $ head indices'
    where indices' = checkIndices indices
          checkIndices []     = []
          checkIndices (idx:idxs) = if (idx !! 2) == '*'
                                    then idxs
                                    else checkIndices idxs
          extractNum = reverse . extractStrUpToSpace . reverse

extractStrUpToSpace :: String -> String
extractStrUpToSpace []      = []
extractStrUpToSpace (t:txt) | t == ' '  = [] 
                            | otherwise = t:extractStrUpToSpace txt 

setNextIndex :: [Char] -> X ()
setNextIndex index = spawn $ printf "pacmd set-default-sink %s" index

switch :: X ()
switch = do
    sinkData <- listSinks
    let currentIndices = getIndices . breakLines $ sinkData
    let nextIndex = getNextIndex currentIndices
    setNextIndex nextIndex 
