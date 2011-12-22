
module Git.Utils where

whitespaceCharacters :: String
whitespaceCharacters = " \t\r\n"

strip :: String -> String
strip = lstrip . rstrip

lstrip :: String -> String
lstrip s = case s of
    []     -> []
    (x:xs) -> if elem x whitespaceCharacters then lstrip xs else s

rstrip :: String -> String
rstrip = reverse . lstrip . reverse
