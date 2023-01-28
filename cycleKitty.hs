{-
    Script to cycle through the colors for the kitty terminal.
    The current theme will be stored in '~/.config/kitty/currentTheme.txt' and
    that value will be retrieved to determine the next theme option and to set
    that next theme value.
-}
import qualified Control.Monad         as M
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC
import           Data.Foldable         (forM_)
import qualified Data.List             as L
import           Data.Maybe
import qualified Data.Text             as T
import qualified System.Directory      as D
import           System.FilePath
import           System.IO
import           System.Process
​
​
-- Determines which direction to cycle themes
data Direction = Up | Down
​
​
-- File where current theme is stored
kittyFile :: IO String
kittyFile = do
    home <- D.getHomeDirectory
    return $ home </> ".config/kitty/currentTheme.txt"
​
​
-- Directory where the themes are stored
kittyThemeDirectory :: IO String
kittyThemeDirectory = do
    home <- D.getHomeDirectory
    return $ home </> ".config/kitty/kitty-themes/themes"
​
​
getCurrentKittyTheme :: IO BC.ByteString
getCurrentKittyTheme = do
    kittyFileExists <- kittyFile >>= D.doesFileExist
    M.unless kittyFileExists $ do
       themes <- getThemeList
       setCurrentKittyTheme $ head themes
    kittyFile >>= BS.readFile
​
​
setCurrentKittyTheme :: String -> IO ()
setCurrentKittyTheme theme = do
    file <- kittyFile
    BS.writeFile file $ BC.pack theme
​
​
getThemeList :: IO [String]
getThemeList = L.sortOn (T.toLower . T.pack) <$>
    (kittyThemeDirectory >>= D.listDirectory)
​
​
nextTheme :: Direction -> String -> [String] -> Maybe String
nextTheme dir theme themeList = do
    currentIndex <- L.elemIndex theme themeList
    let newIndex = case dir of
                     Up -> if (currentIndex - 1) < 0
                       then length themeList - 1
                       else currentIndex - 1
                     Down -> if (currentIndex + 1) == length themeList
                       then 0
                       else currentIndex + 1
    let newTheme = themeList !! newIndex
    return newTheme
​
​
setKittyTheme :: String -> IO ()
setKittyTheme theme = do
    dir <- kittyThemeDirectory
    let themePath = makeValid $ dir </> theme
    _ <- callCommand $ "kitty @ set-colors -a \"" ++ themePath ++ "\""
    return ()
​
​
cycleTheme :: Direction -> IO ()
cycleTheme dir = do
    currentTheme <- getCurrentKittyTheme
    themes <- getThemeList
    let maybeNextTheme = nextTheme dir (BC.unpack currentTheme) themes
    if isJust maybeNextTheme
        then
            do
                let nextTheme = fromJust maybeNextTheme
                setKittyTheme nextTheme
                putStrLn nextTheme
                setCurrentKittyTheme nextTheme
        else
            putStr "No themes found."
​
​
mapDirection :: Char -> Maybe Direction
mapDirection c
  | c == 'j' = return Down
  | c == 'k' = return Up
  | c == 'n' = return Down
  | c == 'e' = return Up
  | otherwise = Nothing
​
​
main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    M.forever $ do
        maybeDir <- mapDirection <$> getChar
        forM_ maybeDir cycleTheme
