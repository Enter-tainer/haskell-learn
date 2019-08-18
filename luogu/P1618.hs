module P1618 where
import           Data.List
import           Data.Foldable
import           Text.Printf
import           Data.Char
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as I

check :: [Int] -> Bool
check xs = "123456789" == sort (concat (show <$> xs))


int :: String -> Int
int str = int' (filter (not . isSpace) str) 0
 where
  int' []         x = x
  int' ('-' : xs) _ = -1 * (int' xs 0)
  int' (x   : xs) p = int' xs $ p * 10 + digitToInt x

readToList :: IO [Int]
readToList = map (int . T.unpack) . T.words <$> I.getLine

solve :: (Int, Int, Int) -> [(Int, Int, Int)]
solve (x, y, z) =
  [ (x * a, y * a, z * a)
  | a <- [1 .. 329] :: [Int]
  , check [x * a, y * a, z * a]
  ]

main :: IO ()
main = do
  xs <- readToList
  let [x, y, z] = map (\t -> t `div` (foldl1' gcd xs)) xs
  let res = solve (x, y, z)
  if null res
    then printf "No!!!"
    else for_ res $ \(a, b, c) -> printf "%d %d %d\n" a b c
  return ()
