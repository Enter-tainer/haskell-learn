module P1047 where
import           Control.Monad
import           Data.Array.ST
import           Data.Array.Unboxed
import           Data.Char          (digitToInt, isSpace)
import           Data.Foldable
import qualified Data.Text          as T
import qualified Data.Text.IO       as I
import           Text.Printf        (printf)

int :: String -> Int
int str = int' (filter (not . isSpace) str) 0
  where
  int' []         x = x
  int' ('-' : xs) _ = -1 * int' xs 0
  int' (x   : xs) p = int' xs $ p * 10 + digitToInt x

readToList :: IO [Int]
readToList = map (int . T.unpack) . T.words <$> I.getLine

setRanges :: Int -> [(Int, Int)] -> [Bool]
setRanges _ [] = []
setRanges n xs = elems $ runSTUArray $ do
  res <- newArray (0, n) True
  for_ xs $ \(l, r) ->
    for_ ([l..r]::[Int]) $ \i ->
      writeArray res i False
  return res

main :: IO ()
main = do
  [l, n] <- readToList 
  xs <- replicateM n readToList
  let res = setRanges l $ (\[a, b] -> (a, b)) <$> xs
  printf "%d" (length $ filter (==True) res)
  return ()