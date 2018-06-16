module Codewars.Kata.Permutations where
import Data.List (inits, tails, group, sort)
permutations :: String -> [String]
permutations = map head . group . sort . mpermutations
-- permutations = map head . L.group . L.sort . L.permutations
mpermutations :: String -> [String]
mpermutations (x:xs) = concat $ map (insertEveryplace x) (permutations xs)
mpermutations [] = [[]]

insertEveryplace :: Char -> String -> [String]
insertEveryplace c xs = foldl (\acc x -> (fst x ++ [c] ++ snd x) : acc) [] (zip xinit xtail)
  where xinit = inits xs
        xtail = tails xs

-- tails :: (Ord a) => [a] -> [[a]]
-- tails [] = [[]]
-- tails xs = xs : (tails $ tail xs)

-- inits :: (Ord a) => [a] -> [[a]]
-- inits [] = [[]]
-- inits xs = xs : (inits $ init xs)