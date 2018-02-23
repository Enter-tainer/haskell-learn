module Codewars.Kata.Categorize where
import Codewars.Kata.Categorize.Types
-- data Membership = Open | Senior deriving (Eq, Show)
openOrSenior :: [(Int, Int)] -> [Membership]
openOrSenior [] = []
openOrSenior ((age, hand):xs)
    | age >= 55 && hand > 7 = Senior : (openOrSenior xs)
    | otherwise = Open : (openOrSenior xs)
-- Another solution (better)
openOrSenior = map (\(a,b) -> if a >= 55 && b > 7 then Senior else Open)