{-# OPTIONS_GHC -O2 #-}
module P1035 where
import           Data.Char    (digitToInt, isSpace)
import qualified Data.Text    as T
import qualified Data.Text.IO as I
import           Text.Printf  (printf)

int :: String -> Int
int str = int' (filter (not . isSpace) str) 0
  where
  int' []         x = x
  int' ('-' : xs) _ = -1 * int' xs 0
  int' (x   : xs) p = int' xs $ p * 10 + digitToInt x

readToList :: IO [Int]
readToList = map (int . T.unpack) . T.words <$> I.getLine

prefixSum :: (Num a) => [a] -> [a]
prefixSum = prefixSum' 0
  where
    prefixSum' _ [] = []
    prefixSum' tot (t:ts) = (tot + t):prefixSum' (tot + t) ts

solve :: Int -> Int
solve k = fst $ head $ filter (\(_, v) -> v > kk) $ zip nat ys
  where
    nat = [1..] :: [Int]
    kk = fromIntegral k :: Double
    xs = [1 / v | v <- (fromIntegral <$> nat)]
    ys = prefixSum xs

main :: IO ()
main = do
  [n] <- readToList
  printf "%d" $ solve n
  return ()
