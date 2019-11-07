module JsonParser where

import MonadicParsing
import StateTransformer
import Control.Applicative

data Json = Brackets [Pair] | V Value
  deriving (Show, Eq)
type Pair = (String, Value)
data Value = Str String | Val Int | Flt Float| JN Json | Array [Value] deriving (Show, Eq)

{-========================================================-}
ex0 = Brackets []
ex1 = Brackets [("z", JN (Brackets [("a", Array [Val 10,Val 11, Val 12]), ("b", Array [Val 2, Val 3, Val 4]), ("c", Str "Hello")]))]
{-{ "z": {"a": [10,11,12], "b": [2,3,4], "c":"Hello"} }-}
ex2 = Brackets [("z", JN (Brackets [("a", Array [Val 10, Val 11, Val 12]), ("b", Array [Val 13, Val 14, JN (Brackets [("a", Val 15)])]), ("c", JN (Brackets [("a", Array [Val 16, Val 17, Val 18])]))]))]
{-{"z": {"a":[10,11,12], "b": [13,14, {"a": 15}], "c":{"a": [16,17,18]}}}-}
ez1 = Brackets [("z", Str "Hello")]
ez1' = "{z:Hello}"
ez2 = Brackets [("z", Val 133)]
ez2' = "{z:133}"
ez3 = Brackets [("z", Str "Hello"),("z", Val 144)]
ez3' = "{z:Hello, z:144}"
ez4 = Brackets [("z", Str "Hello"),("a", Val 1442),("b", Flt 123.22),("c", Str "bye")]
ez5 = Brackets [("z", Array [Str "Days", Str "Nights"])]
ez6 = Brackets [("z", Array [Str "AAA", Str "BBB", Str "CCC", Val 444])]
ez7 = Brackets [("z", Array [Str "Days", Str "Nights", Array [Str "Morning", Str "Afternoon"]]),("a", Str "Easy"),("b", Array [Val 12, Val 13, Array [Val 12, Str "aa"]])]
ez8 = Brackets [("z", Array [Str "Days", Val 5]), ("a", JN (Brackets [("aa", Str "Hello"), ("bey", Val 124)]))]
ez9 = Brackets [("z", JN (Brackets [("zz", JN (Brackets [("zzz", JN (Brackets [("zzzz", Array [Val 9, JN (Brackets [("zzzzz", Val 10)])])]))]))]))]
ez10 = Brackets [("a", Array [JN ez1, JN ez2, JN ez2, JN ez2])]
{-=========================================================-}

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
encode (Brackets listOfPairs) = "{"++ pairsToString listOfPairs ++ "}"

pairsToString :: [Pair] -> String
pairsToString [] = ""
pairsToString ((name, value):[]) = show name++ ":" ++ valueToString value
pairsToString (x:xs) = pairsToString [x] ++ ", " ++ pairsToString xs

valueToString :: Value -> String
valueToString (Str x) = show x
valueToString (Val x) = show x
valueToString (Flt x) = show x
valueToString (Array xs) ="[" ++ concat(mapComma (fmap valueToString xs)) ++ "]"
valueToString (JN (Brackets (x:xs)))= "{" ++ concat(map pairsToString [(x:xs)]) ++ "}"

mapComma :: [String] -> [String]
mapComma [] = []
mapComma (x:[]) = [x]
mapComma (x:xs) = ((x ++ ",") : mapComma xs)

{-===========================================================-}

k0 = "\"" ++ "     Hello " ++ "\""
v0 = "\"" ++ " bye " ++ "\""
hd0 = "{" ++ k0 ++ ":" ++ v0 ++ "}"
pair0 = k0 ++ ":" ++ v0
hd1 = "{" ++ k0 ++ ":" ++ "  123.321" ++ "}"
pair1 = k0 ++ ":" ++ "123.321"
hd2 = "{" ++ pair0 ++ "," ++ pair1 ++ "," ++ pair1 ++ "}"
v3 = "[   123.321,    123, 35," ++ v0 ++ "]"
pair3 = k0 ++ ":" ++ v3
hd3 = "{" ++ pair3 ++ "}"
pair4 = k0 ++ ":" ++ hd0
hd4 = "{" ++ pair4 ++ "}"


json :: Parser Json
json = do
  res <- parseBrackets
  return res

parseBrackets :: Parser Json
parseBrackets = do
  symbol "{"
  currPair <- parsePair
  remPair <- many (do
                      symbol ","
                      parsePair)
  symbol "}"
  return (Brackets(currPair:remPair))

parsePair :: Parser Pair
parsePair = do
  symbol "\""
  key <- anyStr
  symbol "\""
  symbol ":"
  value <- parseValue
  return ((key, value))

parseValue :: Parser Value
parseValue = do
  parseStr
  <|>
  parseFlt
  <|>
  parseVal
  <|>
  parseJN
  <|>
  parseArray

parseVal :: Parser Value
parseVal = do
  int <- natural
  return (Val int)

parseFlt :: Parser Value
parseFlt = do
  flt <- parseFloat
  return (Flt flt)

parseStr :: Parser Value
parseStr = do
  symbol "\""
  str <- anyStr
  symbol "\""
  return (Str str)

parseArray :: Parser Value
parseArray = do
  symbol "["
  curr <- parseValue
  rem <- many (do
                  symbol ","
                  parseValue)
  symbol "]"
  return (Array (curr:rem))

parseJN :: Parser Value
parseJN = do
  item <- parseBrackets
  return (JN item)

ident' :: Parser String
ident'= do
  x <- alphanum
  xs <- many alphanum
  return (x:xs)

ident'' :: Parser String
ident'' = do
  x <- digit
  xs<- many digit
  return (x:xs)

parseFloat :: Parser Float
parseFloat = do
  n0 <- ident''
  n1 <- symbol "."
  n2 <- ident''
  return (read (n0++n1++n2) :: Float)

anyStr :: Parser String
anyStr = token ident'

{-===================================================================-}
-- hint: you will need to define lots of grammar components
--       e.g., for recognizing JSON objects, arrays, etc.
--
--       Since you cntrol the implementation of Json data type
--       try starting with a subset of JSON and build up gradually,
--       testing your encode and decode

-- Querying a Json

-- given a Json (object), return the list of keys at the top level
listTopLevelKeys :: Json -> [String]
listTopLevelKeys (Brackets []) = []
listTopLevelKeys (Brackets ((key,value):xs)) = ((key): (listTopLevelKeys (Brackets xs)))

-- given a Json, return the list of all keys in the data structure
listKeys :: Json -> [String]
listKeys (Brackets xs) = listTopLevelKeys (Brackets xs) ++ concatMap getKey (filtered (fmap snd xs))

filtered :: [Value] -> [Value]
filtered [] = []
filtered (JN (Brackets ns):xs) = ((JN (Brackets ns)): filtered xs)
filtered ((Array ns):xs) = ((Array ns):filtered xs)
filtered (_:xs) = filtered xs

getKey :: Value -> [String]
getKey (JN (json)) = listKeys json
getKey (Array xs) = concatMap getKey (filtered xs)

{-=====================================================================-}
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
{-===================================================================-}
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
label (Brackets xs) = pure (Brackets) <*> pairLabel xs

pairLabel :: [Pair] -> ST [Pair]
pairLabel (x:[]) = pure (\y -> [y]) <*> valLabel x
pairLabel (x:xs) = pure (\ys y -> y:ys) <*> pairLabel xs <*> valLabel x

valLabel :: Pair -> ST Pair
valLabel (key, Array (x:xs)) = pure (\a b -> (key ++ (show a), b)) <*> fresh <*> (pure Array <*> listLabel ((x:xs)))
valLabel (key, (JN xs)) = pure (\a b -> (key ++ (show a), b)) <*> fresh <*> (pure JN <*> (label xs))
valLabel (key, baseValue) = pure (\a b -> (key ++ (show a) , b)) <*> fresh <*> pure (baseValue)

listLabel :: [Value] -> ST [Value]
listLabel ((JN jn):[]) = pure (\k -> [k]) <*>  (pure JN <*> label jn)
listLabel ((Array array):[]) = pure (\k -> [k]) <*> (pure Array <*> (listLabel array))
listLabel (others:[]) = pure (\k -> [k]) <*> pure (others)
listLabel (x:xs) = pure (\ys y -> y++ys) <*> listLabel xs <*> listLabel [x]
{-listLabel = undefined-}


md = Brackets [("a", Val 1), ("b", Array [JN (Brackets [("c", Str "Bye")])])]
md' = Brackets [("a", Str "hello"), ("b", Array [Array [JN (Brackets [("c", Str "Bye")])]])]
