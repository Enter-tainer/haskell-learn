module P1425 where
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

main :: IO()
main = do
  [a, b, c, d] <- readToList
  let (m ,h) = if d < b then (60 + d - b, c - a - 1) else (d - b, c - a)
  printf "%d %d" h m
  return ()
