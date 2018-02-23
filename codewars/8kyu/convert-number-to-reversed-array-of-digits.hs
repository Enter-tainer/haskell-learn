module Codewars.Kata.Convert where
import Data.Char
digitize :: Int -> [Int]
digitize s = map digitToInt (reverse (show s))