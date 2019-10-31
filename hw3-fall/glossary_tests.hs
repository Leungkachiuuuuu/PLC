import JsonParser
import System.Environment

extract :: Maybe a -> a
extract (Just x) = x

runSearchByKey :: Json -> String -> IO ()
runSearchByKey j k = let msbk = searchByKey k j in
                            do putStr ("search '" ++ k ++ "' (Maybe Json) " ++ (show msbk) ++ "\n")
                               if (msbk == Nothing) 
                                then return () 
                                else putStr ("search '" ++ k ++ "' (JSON) " ++ (encode (extract msbk)) ++ "\n")
                               putStr("\n")

printAllJson :: [Json] -> IO ()
printAllJson [] = return ()
printAllJson (j:js) = do putStr (encode j)
                         printAllJson js

runSearchPath :: [KeyOrIndex] -> Json -> IO ()
runSearchPath q j = let msp = searchPath q j in
                        do putStr ("searchPath " ++ (show q) ++ " ([Json]) " ++ (show msp) ++ "\n")
                           if (msp == []) 
                            then return () 
                            else printAllJson msp
                           putStr("\n")

main = do [fn] <- getArgs
          content <- readFile fn
          putStr "===== Parsing the JSON =====\n"
          let mj = decode content
            in if mj == Nothing then
                putStr "Parser failed to parse the input as JSON\n"
               else let j = extract mj
                    in do putStr ("Json: " ++ (show j) ++ "\n\n")
                          putStr ("JSON: " ++ (encode j) ++ "\n\n")

                          putStr "===== listKeys =====\n"
                          putStr (show (listKeys j) ++ "\n\n")

                          putStr "===== searchByKey =====\n"
                          runSearchByKey j "title"
                          runSearchByKey j "GlossDef"
                          runSearchByKey j "Expects Failure"

                          putStr "===== searchPath =====\n"
                          runSearchPath [Key "glossary", Key "GlossDiv", Key "title"] j
                          runSearchPath [Key "glossary", Key "GlossDiv", Key "GlossList", Key "GlossEntry", Key "GlossDef", Key "GlossSeeAlso", Index 1] j
                          runSearchPath [Key "glossary", Key "GlossDiv", Index 4, Key "Expects Failure", Index 0] j

                          putStr "===== makeKeysUnique =====\n"
                          putStr ("makeKeysUnique (Json) " ++ (show (makeKeysUnique j)) ++ "\n\n")
                          putStr ("makeKeysUnique (JSON) " ++ (encode (makeKeysUnique j)) ++ "\n\n")



