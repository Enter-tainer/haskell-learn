module MaxSequence where

maxSequence :: [Int] -> Int
maxSequence = maximum . scanl (\acc x -> max 0 acc + x) 0