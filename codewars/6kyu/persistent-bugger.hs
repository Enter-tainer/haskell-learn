module Codewars.G.Persistence where
import Data.Char
persistence :: Int -> Int
persistence n
    | n <= 9 = 0
    | otherwise = 1 + (persistence $ product (map digitToInt (show n)))
