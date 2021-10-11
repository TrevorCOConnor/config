data Color = Red | Green | Blue

class BasicEq a where 
    isEqual :: a -> a -> Bool

instance BasicEq Bool where
    isEqual True True   = True
    isEqual False False = True
    isEqual _ _         = False

class BasicEq2 a where
    isEqual2 :: a -> a -> Bool
    isEqual2 x y = not (isNotEqual2 x y)

    isNotEqual2 :: a -> a -> Bool
    isNotEqual2 x y = not (isEqual2 x y)

instance Show Color where
    show Red   = "Red"
    show Blue  = "Blue"
    show Green = "Green"

instance Read Color where
    readsPrec _ value = tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
            where tryParse [] = []
                  tryParse ((attempt, result):xs) =
                        if (take (length attempt) value) == attempt
                           then [(result, drop (length attempt) value)]
                           else tryParse xs
