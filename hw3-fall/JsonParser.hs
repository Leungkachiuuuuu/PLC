module JsonParser where

import MonadicParsing
import StateTransformer
import Control.Applicative

data Json = X {- define yourself -}
  deriving (Show, Eq)

-- given a String, turn it into a Json
-- returns Nothing if it could not be parsed
-- DO NOT modify
decode :: String -> Maybe Json
decode s = let res = parse json s in
               -- result of parse is a list of parses, empty if no parse
               if res == [] then Nothing else Just (fst (res !! 0))

-- given a Json, turn it into a String that is valid JSON
-- Note: This is different from the derived Show for Json.
--       The derived Show will depend on your Json definition
--       while encode has identical output (up to whitespace) for everyone
--
--       You should be able to test this before your Parser even
--       works by constructing example Jsons your self
encode :: Json -> String
encode = undefined

-- Parsing a String into a Json

json :: Parser Json
json = undefined 

-- hint: you will need to define lots of grammar components
--       e.g., for recognizing JSON objects, arrays, etc.
--
--       Since you control the implementation of Json data type
--       try starting with a subset of JSON and build up gradually,
--       testing your encode and decode

-- Querying a Json

-- given a Json (object), return the list of keys at the top level
listTopLevelKeys :: Json -> [String]
listTopLevelKeys = undefined

-- given a Json, return the list of all keys in the data structure
listKeys :: Json -> [String]
listKeys = undefined

-- given a key and a Json, return the value
-- return value is Maybe so that Nothing can indicate no such key exists
-- (if Json contains duplicates of the key, then any of the corresponding
-- values may be returned suffices)
searchByKey :: String -> Json -> Maybe Json
searchByKey = undefined

-- given a list of keys and a Json, return the list of values.
-- for a given result, return Nothing if the key was missing
maySearchAll :: [String] -> Json -> [Maybe Json]
maySearchAll = undefined

-- given a list of keys and a Json, return the list of values
-- return Nothing if any one of the keys is missing
mustSearchAll :: [String] -> Json -> Maybe [Json]
mustSearchAll = undefined

-- data type to be used below (DO NOT modify)
data KeyOrIndex = Key String | Index Int
  deriving (Show,Eq)
  
-- given a list of object keys and array indexes denoting a path, return the value in a list of length 1 (indicates succcess)
-- or empty list to indicate failure (path not found)
searchPath :: [KeyOrIndex] -> Json -> [Json]
searchPath = undefined

{- Given a Json that may have duplicate keys,
return a Json where the keys are de-duplicated by renaming.
If the Json has keys "a", "b", "a", "c", "c" you should rename
them "a0", "b1", "a2", "c3", "c4". It doesn't matter which order
you rename the keys, as long as you use this numbering system.

DO NOT modify: instead, see label below
-}
makeKeysUnique :: Json -> Json
makeKeysUnique j = fst (app (label j) 0)

{- helper function to be used in your implementation of
label -}
fresh :: ST Int
fresh = S (\n -> (n, n+1))

{- relabels the keys in the Json to have a suffix integer.
(see makeKeysUnique). You should use the tree labeling
example in Ch 12.3 or list labeling from lecture 14 as your inspiration (alabel or mlabel).
-}
label :: Json -> ST Json
label = undefined
