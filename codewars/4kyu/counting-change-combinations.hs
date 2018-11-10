module Change where
import Data.List
countChange :: Integer -> [Integer] -> Integer
countChange n ox
  | n == 0 = 1
  | n < 0 = 0
  | null ox = 0
  | otherwise = countChange (n - x) l + countChange n xs
  where
    l@(x : xs) = reverse $ sort ox