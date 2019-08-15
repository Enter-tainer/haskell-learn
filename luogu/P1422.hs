module P1422 where
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

getPrice :: Int -> Double
getPrice n
  | n < 0 = 0
  | n <= 150 = 0.4463 * fromIntegral n
  | n <= 400 = 0.4463 * 150 + 0.4663 * fromIntegral (n - 150)
  | otherwise = 0.4463 * 150 + 0.4663 * 250 + 0.5663 * fromIntegral (n - 400)

main :: IO ()
main = do
  [t] <- readToList
  printf "%.1f" $ getPrice t
  return()