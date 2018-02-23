module Isogram where
import Data.Char
isIsogram :: String -> Bool
isIsogram [] = True
isIsogram (x:xs) = toLower x `notElem` (map toLower xs) && isIsogram xs