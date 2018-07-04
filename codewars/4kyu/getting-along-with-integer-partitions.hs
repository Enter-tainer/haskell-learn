module Codewars.G964.Partition where
import Data.Function()
import Data.List
import Text.Printf
part :: Int -> String
part n = printf "Range: %d Average: %.2f Median: %.2f" range avg (median seq)
  where seq = mynub $ f' n n
        range = (maximum seq - minimum seq) :: Int
        avg = ((fromIntegral (sum seq)) / (fromIntegral (length seq))) :: Double
        median xs = (if odd $ length xs 
                      then fromIntegral (xs !! ((length xs) `div` 2))
                      else fromIntegral ((xs !! ((length xs) `div` 2)) + (xs !! ((length xs) `div` 2 - 1))) / 2) :: Double

f :: Int -> Int -> [[Int]]
f n m
  | m == 1 = [take n [1, 1 ..]]
  | m == n && n == 1 = [[1]]
  | n < m = f n n
  | n == m = [n] : (f n (n - 1))
  | otherwise = (f n (m - 1)) ++ (map ((:)m) (f (n - m) m))

f' :: Int -> Int -> [Int]
f' n m
  | m == 1 = [1]
  | m == n && n == 1 = [1]
  | n < m = f' n n
  | n == m = n : (f' n (n - 1))
  | otherwise = (f' n (m - 1)) ++ (map ((*)m) (f' (n - m) m))

mynub :: (Ord a) => [a] -> [a]
mynub xs = map head $ group $ sort xs

-- A Better Solution
partaux :: Int -> [[Int]]
partaux k = (mem !! k) where
    mem = [[]] : [[x:y | x <- [1..n], y <- mem !! (n - x), [x] >= take 1 y] | n <- [1..k]]