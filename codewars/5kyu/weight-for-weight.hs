module Codewars.G964.WeightSort where
import Data.List
orderWeight :: [Char] -> [Char]
orderWeight str = unwords $ sortBy (\x y -> if weight x == weight y 
                                      then compare x y
                                      else compare (weight x) (weight y)) $ words str 
    where weight x = sum $ map chartoInt $ x
          chartoInt x = read (pure x :: String) :: Int