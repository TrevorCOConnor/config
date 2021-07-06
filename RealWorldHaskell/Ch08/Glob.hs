module Glob (namesMatching) where

import System.Info (os)
import System.Directory (doesDirectoryExist, doesFileExist,
                         getCurrentDirectory, getDirectoryContents)
import System.FilePath (dropTrailingPathSeparator, splitFileName, (</>), equalFilePath)
import Control.Exception (handle)
import Control.Monad (forM)
import GlobRegex (matchesGlob)
import Data.Char (toLower)

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")


caseSensitive :: Bool
caseSensitive = not $ os == "mingw32"


namesMatching :: String -> IO [FilePath]
namesMatching pat
 | not (isPattern pat) = do
     exists <- doesNameExist pat
     return (if exists then [pat] else [])
 | otherwise = do
     case splitFileName pat of
     -- splits the path into directory and filename
        ("", baseName) -> do
        -- if no directory is given, we will use the current directory
            curDir <- getCurrentDirectory
            listMatches curDir baseName
        (dirName, baseName) -> do
            dirs <- if isPattern dirName
                       then namesMatching (dropTrailingPathSeparator dirName)
                       -- if directory has pattern specifications,
                       -- run this function on the directory
                       else return [dirName]
                       -- otherwise, just return the directory name
                       --
                       -- either way, we will pull out the IO result to use
                       -- we are essentially recursively going through the path
                       -- finding everything that matches in a tree structure.
            let listDir = if isPattern baseName
                             then listMatches
                             else listPlain
            pathNames <- forM dirs $ \dir -> do
                            baseNames <- listDir dir baseName
                            return (map (dir </>) baseNames)
            return (concat pathNames)


doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
    fileExists <- doesFileExist name
    if fileExists
       then return True
       else doesDirectoryExist name


listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
    dirName' <- if null dirName
                   -- null lst = True if lst is empty
                   then getCurrentDirectory
                   else return dirName
                -- function uses current directory if input is empty string
    handle ((const (return [])) :: IOError -> IO [String]) $ do
        -- If this is correct, then the `const` function here is a very clever way
        -- to ignore the error.
        names <- getDirectoryContents dirName'
        let names' = if isHidden pat
                        then filter isHidden names
                        else filter (not . isHidden) names
                        -- if specified as hidden, then only grab files that start with '.'
        let fixCase = if caseSensitive
                      then id
                      else map toLower
        let caseNames = map fixCase names'
        let casePat = fixCase pat
        return (filter (`matchesGlob` casePat) caseNames)


isHidden :: [Char] -> Bool
isHidden ('.':_) = True
isHidden _       = False


listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
    exists <- if null baseName
                 -- then input is dir
                 then doesDirectoryExist dirName
                 else doesNameExist (dirName </> baseName)
    return (if exists then [baseName] else [])
