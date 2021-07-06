module PutJSON where

import SimpleJSON
import Data.List

renderJValue :: JValue -> String

renderJValue (JString s)   = show s
renderJValue (JNumber n)   = show s
renderJValue (JBool True)  = "true"
renderJValue (JBool False) = "false"
renderJValue (JNull)       = "null"

renderJValue (JObject o) = "{" ++ pairs o ++ "}"
    where pairs [] = ""
          pairs ps = intercalate ", " (map renderPair ps)
          renderPair (k,v) = show k ++ ": " ++ renderJValue v

renderJValue (JArray a) = "[" ++ values a ++ "]"
    where values [] = ""
          values vs = intercalate ", " (map renderValue vs)

putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)
