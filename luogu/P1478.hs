{-# OPTIONS_GHC -O2 #-}
module P1478 where
import           Control.Monad
import           Data.List
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

main :: IO ()
main = do
  [n, s] <- readToList
  h      <- sum <$> readToList
  appl   <-
    filter ((<= h) . fst)
    <$> fmap (\[a, b] -> (a, b))
    <$> replicateM n readToList
  let res = sort $ snd <$> appl
  let (_, rcnt) = foldl'
        (\(sn, cnt) x -> if sn + x > s then (sn, cnt) else (sn + x, cnt + 1))
        (0 :: Int, 0 :: Int)
        res
  printf "%d\n" rcnt
  return ()
