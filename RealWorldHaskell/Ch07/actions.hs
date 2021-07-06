strToAction :: String -> IO ()
strToAction input = putStrLn ("Data: " ++ input)

listToActions :: [String] -> [IO ()]
listToActions = map strToAction

numbers :: [Int]
numbers = [1..10]

strings :: [String]
strings = map show numbers

actions :: [IO ()]
actions = listToActions strings

printItAll :: IO ()
printItAll = runall actions

runall :: [IO ()] -> IO ()
runall [] = return ()
runall (a:as) = do a
                   runall as

main = do strToAction "Start of the program"
          printItAll
          strToAction "Done!"


-- the layout of this file is fairly convoluted, I can see where the resistance to this structure
-- of functional programming comes from.
