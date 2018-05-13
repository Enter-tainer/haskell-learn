module RangeExtractor.JorgeVS.Kata where
import Data.List
solution :: [Int] -> String
solution myList = concat $ intersperse "," $ trans $ filter (not . null) $ slice myList
trans :: [[Int]] -> [String]
trans [] = []
trans (x:xs)
    | length x == 1 = (concat $ map show x) : trans xs
    | length x == 2 = (concat $ intersperse "," $ map show x) : trans xs
    | otherwise = ((show $ head x) ++ "-" ++ (show $ last x)) : trans xs
slice :: [Int] -> [[Int]]
slice [] = [[]]
slice [x] = [[x]]
slice xs = prev : (slice others)
    where shift lst = zip lst $ tail lst
          (p, o) = span (\(a, b) -> a + 1 == b) (shift xs)
          prev = case p of [] -> [fst $ head o]
                           _  -> pp
          others = case oo of [] -> []
                              oo -> tail $ oo
          oo = map fst o ++ [snd $ last o]
          pp = map fst p ++ [snd $ last p]