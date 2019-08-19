{-# OPTIONS_GHC -O2 #-}
module P1464 where

import           Control.Monad
import           Data.Maybe
import           Data.Char                      ( digitToInt
                                                , isSpace
                                                )
import           Data.Int
import           Text.Printf                    ( printf )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as I
import           System.IO.Unsafe
import           System.Exit
import           Data.IORef
import qualified Data.Map                      as Map


int :: (Num a) => String -> a
int str = int' (filter (not . isSpace) str) 0
 where
  int' :: (Num a) => String -> a -> a
  int' []         x = x
  int' ('-' : xs) _ = -1 * (int' xs 0)
  int' (x   : xs) p = int' xs $ p * 10 + (fromIntegral $ digitToInt x)

readToList :: (Num a) => IO [a]
readToList = map (int . T.unpack) . T.words <$> I.getLine

mem :: IORef (Map.Map (Int64, Int64, Int64) Int64)
mem = unsafePerformIO $ newIORef Map.empty

f :: (Int64, Int64, Int64) -> Int64
f (a, b, c)
  | a <= 0 || b <= 0 || c <= 0 = 1
  | a > 20 || b > 20 || c > 20 = w (20, 20, 20)
  | a < b && b < c = w (a, b, c - 1) + w (a, b - 1, c - 1) - w (a, b - 1, c)
  | otherwise = w (a - 1, b, c) + w (a - 1, b - 1, c) + w (a - 1, b, c - 1) - w
    (a - 1, b - 1, c - 1)

w :: (Int64, Int64, Int64) -> Int64
w t = unsafePerformIO $ do
  mp  <- readIORef mem
  res <- if t `Map.member` mp
    then return $ fromJust $ t `Map.lookup` mp
    else do
      let res = f t
      modifyIORef' mem (Map.insert t res)
      return res
  return res

main :: IO ()
main = do
  _ <- forever $ do
    s@(i, j, k) <- (\[a, b, c] -> (a, b, c)) <$> readToList
    if (i == j && j == k && k == -1)
      then exitSuccess
      else printf "w(%lld, %lld, %ld) = %lld\n" i j k (w s)
    return ()
  return ()
