{-# OPTIONS_GHC -O2 #-}
module P1036 where

import           Data.STRef
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
isPrime 2 = True
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

solve :: [Int] -> Int -> [Int]
solve xs k = filter isPrime ys
  where
    ys = sum <$> (filter ((==k) . length) $ subsequences xs)

main :: IO ()
main = do
  [_, k] <- readToList
  xs <- readToList
  printf "%d\n" $ length $ solve xs k
  return ()
