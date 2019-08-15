module P1085 where
import           Data.Char    (digitToInt, isSpace)
import           Data.List
import qualified Data.Text    as T
import qualified Data.Text.IO as I
import           Text.Printf  (printf)

int :: String -> Int
int str = int' (filter (not . isSpace) str) 0
  where
  int' []         x = x
  int' ('-' : xs) _ = -1 * int' xs 0
  int' (x   : xs) p = int' xs $ p * 10 + digitToInt x

lineToList :: T.Text -> [Int]
lineToList l = map (int . T.unpack) $ T.words l

readToList :: IO [Int]
readToList = map (int . T.unpack) . T.words <$> I.getLine

main :: IO()
main = do
  s <- fmap (map lineToList) T.lines <$> I.getContents
  let t = zip ([1..7]::[Int]) $ map sum s
  let e = sortBy (\(_, v) (_, u) -> compare u v) t
  printf "%d" $ fst $ head e
  return ()
