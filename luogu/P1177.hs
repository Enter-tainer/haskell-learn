{-# OPTIONS_GHC -O2 #-}
{-# language Strict #-}
module P1177 where
import Control.Monad
import Data.Char (digitToInt, isSpace)
import Data.List
import Text.Printf (printf)
import qualified Data.Text as T
import qualified Data.Text.IO as I

int :: String -> Int
int str = int' (filter (not . isSpace) str) 0
  where
    int' [] x = x
    int' ('-':xs) _ = -1 * (int' xs 0)
    int' (x:xs) p = int' xs $ p * 10 + digitToInt x

msort :: (Ord a) => [a] -> [a]
msort [] = []
msort (x:xs) = msort l ++ [x] ++ msort r
  where
    l = filter (<=x) xs
    r = filter (>x) xs

printRes :: Int -> IO ()
printRes = printf "%d "

main :: IO ()
main = do
  _ <- int . T.unpack <$> I.getLine
  xs <- map (int . T.unpack) . T.words <$> I.getLine
  let res = sort xs
  forM_ res printRes
