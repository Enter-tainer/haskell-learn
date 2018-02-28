module Codewars.G964.Sumdivsq where

work :: Int -> Int
work x = if isSqr $ fromIntegral ans then ans else -1
    where isSqr x = fromInteger (floor $ sqrt x) ^ 2 == x
          ans = sum $ map (^2) [qwq | qwq <- [1, 2..x], x `mod` qwq == 0]

listSquared :: Int -> Int -> [(Int, Int)]
listSquared m n = [(qwq, work qwq) | qwq <- [m, m+1..n ], work qwq /= -1]
