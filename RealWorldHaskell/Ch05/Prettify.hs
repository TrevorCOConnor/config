module Prettify where

import Prelude hiding ((<>))

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

data Doc = Empty
         | Char Char
         | Text String
         | Line
         | Concat Doc Doc
         | Union Doc Doc
            deriving (Show, Eq)

empty :: Doc
empty = Empty

char :: Char -> Doc
char c = Char c

text :: String -> Doc
text "" = Empty
text s  = Text s

double :: Double -> Doc
double d = text (show d)

line :: Doc
line = Line

(<>) :: Doc -> Doc -> Doc
Empty <> y = y
x <> Empty = x
x <> y     = x `Concat` y

concat :: [[a]] -> [a]
concat = foldr (++) []

hcat :: [Doc] -> Doc
hcat = fold (<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
              case d of
                Empty        -> transform ds
                Char c       -> c : transform ds
                Text s       -> s ++ transform ds
                Line         -> '\n' : transform ds
                a `Concat` b -> transform (a:b:ds)
                _ `Union` b  -> transform (b:ds)

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
    where best col (d:ds) = 
            case d of
                Empty        -> best col ds
                Char c       -> c : best (col + 1) ds
                Text s       -> s ++ best (col + length s) ds
                Line         -> '\n' : best 0 ds
                a `Concat` b -> best col (a:b:ds)
                a `Union` b  -> nicest col (best col (a:ds))
                                           (best col (b:ds))
          best _ _ = ""

          nicest col a b | (width - least) `fits` a = a
                         | otherwise                = b
                       where least = min width col

fits :: Int -> String -> Bool 
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs 

docToList :: Doc -> [Doc]
docToList d = toList [d]
    where toList [] = []
          toList (d:ds) =
              case d of 
                a `Concat` b -> toList (a:b:ds)
                x            -> x:(toList ds)

fill :: Int -> Doc -> Doc
fill width x = hcat (scanLines 0 [x <> line])
    where 
        scanLines _ []       = [] 
        scanLines col (d:ds) =
              case d of
                  Empty        -> scanLines col ds
                  Char c       -> Char c : scanLines (col + 1) ds
                  Text s       -> Text s : scanLines (col + length s) ds
                  Line         -> Text (replicate (width - col) '#') : Line : scanLines 0 ds
                  a `Concat` b -> scanLines col (a:b:ds) 
                  a `Union` b  -> (a `Union` hcat (scanLines col [b])) : 
                                    scanLines (phantomScan col [b]) ds
                     where phantomScan col []   = col
                           phantomScan col (d:ds) = case d of
                                  Empty        -> phantomScan col ds
                                  Char c       -> phantomScan (col + 1) ds
                                  Text s       -> phantomScan (length s) ds
                                  Line         -> phantomScan 0 ds 
                                  a `Concat` b -> phantomScan col (a:b:ds)
                                  _ `Union` b  -> phantomScan col (b:ds)

nest :: Int -> Doc -> Doc
nest width x = hcat (scanLines [] 0 [x])
    where
        scanLines _  _ []            = []
        scanLines indents col (d:ds) =
            case d of 
              Empty        -> scanLines indents col ds
              Char c       -> Char c : scanLines indents' col' ds
                    where (indents', col') = modIndent indents col c
              Text s       -> Text s : scanLines indents (col + length s) ds
              Line         -> addIndent indents Line : scanLines indents 0 ds
              a `Concat` b -> scanLines indents col (a:b:ds)
              _ `Union` b  -> scanLines indents col (b:ds) 
              where addIndent [] doc   = doc 
                    addIndent (n:ns) doc = doc `Concat` Text (replicate n ' ')
                    modIndent indents col c | c == '{'  = ((col:indents), col)
                                            | c == '['  = ((col:indents), col)
                                            | c == ']'  = (tail indents, col)
                                            | c == '}'  = (tail indents, col)
                                            | otherwise = (indents, col + 1)

betterNest :: Int -> Doc -> Doc
betterNest tabWidth x = hcat (scanLines 0 [x])
    where 
        scanLines _ []        = []
        scanLines tabs (d:ds) =
            case d of
                Empty        -> scanLines tabs ds
                Char c       -> Char c : scanLines (modTabs tabs c) ((modChar c):ds)
                Text s       -> Text s : scanLines tabs ds
                Line         -> addTabs tabWidth tabs ds Line : scanLines tabs ds
                a `Concat` b -> scanLines tabs (a:b:ds)
                _ `Union` b  -> scanLines tabs (b:ds)
            where modChar c | c == '{'  = Line
                            | c == '['  = Line
                            | otherwise = Empty
                  modTabs tabs c | c == '{' = tabs + 1
                                 | c == '[' = tabs + 1
                                 | c == ']' = tabs - 1
                                 | c == '}' = tabs - 1
                                 | otherwise = tabs
                  addTabs tabWidth tabs ds line =
                      line `Concat` Text (replicate ((tabs' ds)*(tabWidth)) ' ')
                      where tabs' []     = tabs
                            tabs' (d:ds) = 
                              case d of 
                                Char c -> if c == '}' || c == ']'
                                            then tabs - 1 
                                            else tabs
                                _      -> tabs

betterNest2 :: Int -> Doc -> Doc
betterNest2 tab x = hcat (transform [] 0 [x])
    where transform indents col []               = []
          transform indents col (Line:(Char c):ds) = 
              Line : transform indents' 0 ((addIndent indents'):(Char c):ds)
            where indents' = if closeChar c then tail indents else indents
          transform indents col (d:ds) =
              case d of
                Empty        -> transform indents col ds
                Char c       -> d : transform indents' (col + tab') (charBreak:ds)
                    where indents'  = if openChar c then (col + tab):indents else indents
                          tab'      = if openChar c then tab else 1
                          charBreak = if openChar c then Line else Empty
                Text s       -> d : transform indents (col + length s) ds
                Line         -> d : transform indents 0 (addIndent indents:ds) 
                a `Concat` b -> transform indents col (a:b:ds)
                _ `Union` b  -> transform indents col (b:ds)

          addIndent []     = Empty
          addIndent (d:ds) = Text (replicate d ' ')

openChar :: Char -> Bool
openChar c = (c == '{' || c == '[')

closeChar :: Char -> Bool
closeChar c = (c == '}' || c == ']')

prettyNest :: Int -> Doc -> String
prettyNest tab x = pretty 30 $ betterNest2 tab x
