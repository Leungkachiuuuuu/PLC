module JsonParser where

import MonadicParsing
import StateTransformer
import Control.Applicative
import Data.Char

data Json = Bracket[Dictionary] | Val Value  {- define yourself -}
  deriving (Show, Eq)
type Dictionary = (String,Value)
--data Key = String deriving (Show,Eq)
data Value = I Int | F Float | B Bool | Null | J Json | St String | Array[Value] deriving (Show, Eq)


--test :: Json -> String
--test (Bracket(x:xs)) = show x

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
encode (Bracket(xs)) = "{" ++ helper xs ++"}"

helper' :: Value -> String
helper' (I a) = show a
helper' (F a) = show a
helper' (B a) = fmap toLower (show a)
helper' (Null) = show Null
helper' (St a) = "\"" ++ a ++ "\""
helper' (J xs) = encode (xs)
helper' (Array[]) = ""
helper' (Array(xs)) = "[" ++ (init(foldr (\a b->a++","++b) ("") (fmap helper' xs))) ++ "]"

helper :: [(String,Value)] -> String
helper (x:xs) = "\"" ++ (fst x)++ "\"" ++ ":" ++ helper' (snd x) ++ if xs == [] then "" else "," ++ helper xs



-- Parsing a String into a Json
--newtype Parser a = P (String ->[(a,String)])
json :: Parser Json
json = do
  result <- parseBracket
  return result
--P (\inp -> case inp of
--           ('{':xs) -> [(Bracket[xs],xs)])

-- hint: you will need to define lots of grammar components
--       e.g., for recognizing JSON objects, arrays, etc.
--
--       Since you control the implementation of Json data type
--       try starting with a subset of JSON and build up gradually,
--       testing your encode and decode


--helper functions in Parser Json

parseBracket :: Parser Json
parseBracket = do
  symbol "{"
  dictionary <- parseDictionary
  dictionaries <- many (do
    symbol ","
    parseDictionary)
  symbol "}"
  return (Bracket(dictionary:dictionaries))



parseDictionary :: Parser Dictionary
parseDictionary = do
  symbol"\""
  key <- identifier
  symbol"\""
  symbol ":"
  value <- parseValue
  return (key,value)


parseValue :: Parser Value
parseValue = do
  parseList
  <|>
  parseJson
  <|>
  parseInt
  <|>
  parseString
  <|>
  parseBool
  <|>
  parseNull

parseInt :: Parser Value
parseInt = do
  int <- integer
  return (I int)

parseBool :: Parser Value
parseBool = do
  parseTrue
  <|>
  parseFalse

parseTrue :: Parser Value
parseTrue = do
  bool <- symbol "true"
  return (B (True))

parseFalse :: Parser Value
parseFalse = do
  bool <- symbol "false"
  return (B (False))


parseNull :: Parser Value
parseNull = do
  nullv <- symbol "Null"
  return(Null)

parseString :: Parser Value
parseString = do
  symbol "\""
  string <- takeString
  symbol "\""
  return (St string)

parseFloat :: Parser Value
parseFloat = do
  x <- digits
  y <- symbol "."
  z <- digits
  return (F (read (x++y++z) :: Float))

parseJson :: Parser Value
parseJson = do
  json <- parseBracket
  return (J json)

chars :: Parser String
chars= do
  x <- alphanum
  xs <- many alphanum
  return (x:xs)

digits :: Parser String
digits = do
  x <- digit
  xs <- many digit
  return (x:xs)

inotQuote :: Parser String
inotQuote = do
           x <- notQuote
           xs <- many notQuote
           return (x:xs)


takeString :: Parser String
takeString = token inotQuote

parseList :: Parser Value
parseList = do
  symbol "["
  n <- parseValue
  ns <- many (do
    symbol ","
    parseValue)
  symbol "]"
  return (Array(n:ns))

-- Querying a Json
ex6 = Bracket[("a",Null)]
ex5 = Bracket[("a",B True),("b",I 3)]
ex4 = Bracket[("z",I 9),("zz",I 10)]
-- given a Json (object), return the list of keys at the top level
listTopLevelKeys :: Json -> [String]
listTopLevelKeys (Bracket(xs))= fmap fst xs
listTopLevelKeys _ = []
-- given a Json, return the list of all keys in the data structure

allpair :: Json -> [(String,Value)]
allpair (Bracket((a,J b):xs)) = [(a,J b)] ++ allpair b ++ allpair (Bracket(xs))
allpair (Bracket((d,Array(xs)):xd)) = [(d,(Array(xs)))] ++ helpfindpair xs ++ allpair (Bracket(xd))
allpair (Bracket(x:xs)) = [x] ++ allpair (Bracket(xs))

allpair (Bracket[]) = []

helpfindpair :: [Value] -> [(String,Value)]
helpfindpair ((J b):xs)= allpair b ++ helpfindpair xs
--helpfindpair (I c) = [()]
helpfindpair ((Array(xs):xd)) = helpfindpair xs ++ helpfindpair xd
helpfindpair ((_):xs) = helpfindpair xs
helpfindpair [] = []

listKeys :: Json -> [String]
listKeys json = fmap fst (allpair json)
-- given a key and a Json, return the value
-- return value is Maybe so that Nothing can indicate no such key exists
-- (if Json contains duplicates of the key, then any of the corresponding
-- values may be returned suffices)

ex = Bracket[("a", B True), ("b", Null)]
ex1 = Bracket[("b",J ex)]
ex2 = Bracket[("c",J ex1),("d",St "sadf")]

-- data type to be used below (DO NOT modify)
data KeyOrIndex = Key String | Index Int
  deriving (Show,Eq)

  -- given a list of object keys and array indexes denoting a path, return the value in a list of length 1 (indicates succcess)
  -- or empty list to indicate failure (path not found)
getpair :: Json -> [(String,Value)]
getpair (Bracket[x]) = [x]
getpair (Val (J (Bracket[x]))) = [x]
searchPath :: [KeyOrIndex] -> Json -> [Json]
searchPath xs json = if (elem Nothing (helppath xs json)) then [] else map takeoff (helppath xs json)

helppath :: [KeyOrIndex] -> Json -> [Maybe Json]
helppath ((Key x):xs) json= if elem x (listkeyinlayer json) == True then [Just (output)] ++ (helppath xs output) else [Nothing] where output = (Val (snd (head (filter (\y->fst y == x) (getpair json) ))))
helppath ((Index x):xs) json= if length(listkeyinlayer json) <= x+1 then [Just (output)] ++ (helppath xs output) else [Nothing] where output = (Val (snd (((getpair json)!! x))))
helppath [] json = []

listkeyinlayer :: Json -> [(String)]
listkeyinlayer (Bracket(xs)) = map fst xs
listkeyinlayer (Val (J (Bracket(xs)))) = map fst xs
listkeyinlayer _ = []
-- given a list of object keys and array indexes denoting a path, return the value in a list of length 1 (indicates succcess)
-- or empty list to indicate failure (path not found)
searchByKey :: String -> Json -> Maybe Json
searchByKey str json = if (elem str (map fst (allpair json)) == False) then Nothing else Just (Val (snd (head(filter (\x -> (fst x)==str ) (allpair json)))))


maySearchAll :: [String] -> Json -> [Maybe Json]
maySearchAll (str) json = if length (filter (\x-> x /= Nothing) output) /= 0 then filter (\x->x/= Nothing) output else [Nothing] where output = helper_maysearch (str) json

helper_maysearch :: [String] -> Json -> [Maybe Json]
helper_maysearch (s:str) json = if (elem s (listKeys json) ==False) then [Nothing]
  else map Just (map (Val) (map snd (filter (\x-> (fst x) == s) (allpair json)))) ++ helper_maysearch str json
helper_maysearch [] _ = []
-- given a list of keys and a Json, return the list of values
-- return Nothing if any one of the keys is missing
mustSearchAll :: [String] -> Json -> Maybe [Json]
mustSearchAll (str) json= if elem Nothing output then Nothing else Just (map (takeoff) output) where output = helper_maysearch str json
--mustSearchAll = undefined
takeoff :: Maybe Json -> Json
takeoff (Just x) = x
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
label (Bracket xs) = pure (Bracket) <*> listlabel xs

listlabel :: [Dictionary] -> ST [Dictionary]
listlabel (x:[]) = pure(\k -> [k]) <*> jlabel x
listlabel (x:xs) = pure(\ys y -> y:ys) <*> listlabel xs <*> jlabel x

jlabel :: Dictionary -> ST Dictionary
jlabel (key, Array(x: xs))= pure (\n val -> (key++(show n), val)) <*> fresh <*> (pure(Array) <*> arraylabel (x:xs))
jlabel (key, J (Bracket xs)) = pure (\n val -> (key++(show n), val)) <*> fresh <*> (pure (J) <*> label (Bracket xs))
jlabel (key, value) = pure (\n val -> (key++(show n), val)) <*> fresh <*> pure(value)

arraylabel :: [Value] -> ST [Value]
arraylabel ((J j):[]) = pure (\k -> [k]) <*>  (pure J <*> label j)
arraylabel ((Array array):[]) = pure (\k -> [k]) <*> (pure Array <*> (arraylabel array))
arraylabel (x:[]) = pure (\k -> [k]) <*> pure (x)
arraylabel (x:xs) = pure (\ys y -> y++ys) <*> arraylabel xs <*> arraylabel [x]
  --fmap Leaf(pure(\n -> x++(show n)) <*> fresh)

