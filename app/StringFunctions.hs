module StringFunctions where

splitOnce :: String -> Char -> String
splitOnce "" _ = ""
splitOnce (headChar:rest) delim = if headChar == delim
                             then ""
                             else headChar:splitOnce (rest) delim

splitOnceBySpace :: String -> String
splitOnceBySpace x = splitOnce x ' '

split :: String -> Char -> [String]
split str delim= if delim `elem` str
                   then splittedOnce:split (drop (length splittedOnce + 1) str) delim
                   else [str]
  where splittedOnce = splitOnce str delim

splitBySpace :: String -> [String]
splitBySpace x = split x ' '