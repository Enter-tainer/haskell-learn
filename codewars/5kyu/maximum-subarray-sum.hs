module MaxSequence where

maxSequence :: [Int] -> Int
maximum' [] = 0
maximum' x = maximum x
maxSequence s = maximum' (foldl (\acc x -> if head acc + x < 0 then 0:acc else head acc + x:acc) [0] s)