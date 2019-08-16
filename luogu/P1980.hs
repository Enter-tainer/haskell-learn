module P1980 where
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

getAcc :: Int -> Int -> Int
getAcc v n = length $ filter (==v) $ digitToInt <$> str
  where
    str = show n

solve :: Int -> [Int] -> Int
solve v = foldl (\acc x -> getAcc v x + acc) 0

main :: IO ()
main = do
  [n, x] <- readToList
  printf "%d" $ solve x ([1..n]::[Int])
  return ()
