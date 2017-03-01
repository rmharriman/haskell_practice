module PrettyJSON where

import Data.List (intercalate)
import SimpleJSON
import Prettify

-- renders JValues as strings

renderJValue :: JValue -> Doc

renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str

renderJValue (JObject o)   = "{" ++ pairs o ++ "}"
    where pairs [] = ""
          pairs ps = intercalate ", " (map renderPair ps)
          renderPair (k,v) = show k ++ ": " ++ renderJValue v

renderJValue (JArray a) = "[" ++ values a ++ "]"
    where values [] = ""
          values vs = intercalate ", " (map renderJValue vs)

putJValue :: JValue -> IO ()
putJValue = putStrLn . renderJValue
