module Fibonacci where
import Data.Vector

fibonacci :: Int -> Integer
fibonacci n = v ! (n) where
    v = generate (n+1) f
    f 0 = 0
    f 1 = 1
    f k = (v ! (k-1)) + (v ! (k-2))

fib' :: Int -> Integer
fib' n = fibVec !! n
    where fibVec = gen (n + 1) fib
          gen len f = map f $ take len [0..]
          fib 0 = 1
          fib 1 = 1
          fib k = (fibVec !! (k - 1)) + (fibVec !! (k - 2))