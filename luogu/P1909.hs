module P1909 where

import           Control.Monad
import           Data.Char     (digitToInt, isSpace)
import qualified Data.Text     as T
import qualified Data.Text.IO  as I
import           Text.Printf   (printf)

int :: String -> Int
int str = int' (filter (not . isSpace) str) 0
  where
  int' []         x = x
  int' ('-' : xs) _ = -1 * int' xs 0
  int' (x   : xs) p = int' xs $ p * 10 + digitToInt x

readToList :: IO [Int]
readToList = map (int . T.unpack) . T.words <$> I.getLine

readToPair :: IO (Int, Int)
readToPair = do
  [a, b] <- readToList
  return (a, b)

getPacks :: Int -> Int -> Int
getPacks n c
  | n `mod` c == 0 = n `div` c
  | otherwise  = n `div` c + 1

main :: IO ()
main = do
  [n] <- readToList
  r <- replicateM 3 readToPair
  printf "%d" (minimum $ map (\(c, v) -> v * getPacks n c) r)
  return ()
