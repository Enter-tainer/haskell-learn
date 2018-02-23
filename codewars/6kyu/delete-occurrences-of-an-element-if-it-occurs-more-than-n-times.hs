module Codewars.Kata.Deletion where
work :: [Int] -> [Int] -> Int -> [Int]
work _ [] _ = []
work pre suc n
    | length [qwq | qwq <- head suc : pre, qwq == head suc] <= n = head suc : work (pre ++ [head suc]) (tail suc) n
    | otherwise = work (pre ++ [head suc]) (tail suc) n
deleteNth :: [Int] -> Int -> [Int]
deleteNth = work [] 