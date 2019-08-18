{-# OPTIONS_GHC -O2 #-}
module P1217 where

import           Data.STRef
import           Data.Foldable
import           Data.List
import           Control.Monad.ST
import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.Char                      ( digitToInt
                                                , isSpace
                                                )
import           Text.Printf                    ( printf )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as I

int :: String -> Int
int str = int' (filter (not . isSpace) str) 0
 where
  int' []         x = x
  int' ('-' : xs) _ = -1 * (int' xs 0)
  int' (x   : xs) p = int' xs $ p * 10 + digitToInt x

readToList :: IO [Int]
readToList = map (int . T.unpack) . T.words <$> I.getLine

loop :: Monad m => MaybeT m a -> m (Maybe b)
loop stat = runMaybeT $ forever stat

isPrime :: (Integral a) => a -> Bool
isPrime x = runST $ do
  rx  <- newSTRef 2
  res <- newSTRef True
  _   <- loop $ do
    v <- lift $ readSTRef rx
    when (x `mod` v == 0) $ do
      lift $ writeSTRef res False
      mzero
    lift $ modifySTRef' rx (+ 1)
    when (v * v >= x) mzero
  readSTRef res

makePal :: Int -> Int
makePal 11 = 11
makePal x  = (10 ^ (1 + getRank x)) * (x `div` 10) + reverseInt x
 where
  reverseInt = (read . reverse . show) :: Int -> Int
  getRank t =
    length $ takeWhile (> 0) [ t `div` (10 ^ r) | r <- [1 ..] :: [Int] ]


getPal :: (Int, Int) -> [Int]
getPal (l, r) =
  [ x | x <- takeWhile (<= r) (makePal <$> [1 ..]), isPrime x, x >= l ]

main :: IO ()
main = do
  [l, r] <- readToList
  for_ (sort $ getPal (l, r)) $ \x -> do
    printf "%d\n" x
  return ()
