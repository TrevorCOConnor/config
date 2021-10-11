main = interact (unlines . filter (elem 'a') . lines)
-- unlines :: [String] -> String
--      joins the list with "\n"
-- lines :: String -> [String]
--      undoes unlines
-- Essentially, how this function works is:
--      it takes in the contents of a file with line breaks
--      turns it into a list of strings
--      filters it
--      and returns it back to a string with line breaks.
