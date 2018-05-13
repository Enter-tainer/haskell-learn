module Fibonacci where
import Data.Vector

fibonacci :: Int -> Integer
fibonacci n = v ! (n) where
    v = generate (n+1) f
    f 0 = 0
    f 1 = 1
    f k = (v ! (k-1)) + (v ! (k-2))