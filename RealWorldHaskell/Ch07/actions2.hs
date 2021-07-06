strToMessage :: String -> String
strToMessage input = "Data: " ++ input

strToAction :: String -> IO ()
strToAction = putStrLn . strToMessage

numbers :: [Int]
numbers = [1..10]

main = do strToAction "Start of the program"
          mapM_ (strToAction . show) numbers
          -- mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
          strToAction "Done!"
