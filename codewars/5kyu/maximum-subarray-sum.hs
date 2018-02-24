module MaxSequence where

maxSequence :: [Int] -> Int
maxSequence s = foldl max 0 (foldl (\acc x -> if head acc + x < 0 then 0:acc else head acc + x:acc) [0] s)