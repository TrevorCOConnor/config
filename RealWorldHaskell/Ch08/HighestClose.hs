import qualified Data.ByteString.Lazy.Char8 as L

closing = readPrice . (!!4) . L.split ','

readPrice :: L.ByteString -> Maybe Int
readPrice str =
    case L.readInt str of
      Nothing   -> Nothing
      Just (dollars, rest) ->
          case L.readInt (L.tail rest) of
            Nothing     -> Nothing
            Just (cents, more) ->
                Just (dollars * 100 + cents)
    -- "21.50" -> [21, '.', '5', '0'] -> 21, ".50"
    --  ".50"  -> ['.', '5', '0'] -> '.', "50"
    --  "50"   -> 50
    --  21 * 100 + 50 = 2150 cents

highestClose = maximum . (Nothing:) . map closing . L.lines
    -- maximum      :: Ord a => [a] -> a
    -- ( Nothing: ) :: [Maybe a] -> [Maybe a]
    -- map closing  :: [L.ByteString] -> [Maybe Int]
    -- L.liness     :: L.Bytestring -> [L.ByteString]

highestCloseFrom path = do
    contents <- L.readFile path
    print (highestClose contents)
