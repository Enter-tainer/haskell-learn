module Balanced.Parens where
import Data.List
balancedParens :: Int -> [String]
balancedParens 0 = [""]
balancedParens x = map head $ group $ sort $ concat $ map (insertEveryplace "()") $ balancedParens $ x - 1

insertEveryplace :: String -> String -> [String]
insertEveryplace c xs = foldl' (\acc x -> (fst x ++ c ++ snd x) : acc) [] (zip xinit xtail)
  where xinit = inits xs
        xtail = tails xs
