module P1089 where
import           Data.Char     (digitToInt, isSpace)
import           Data.Foldable
import           Data.IORef
import qualified Data.Text     as T
import qualified Data.Text.IO  as I
import           System.Exit
import           Text.Printf   (printf)

int :: String -> Int
int str = int' (filter (not . isSpace) str) 0
  where
  int' []         x = x
  int' ('-' : xs) _ = -1 * int' xs 0
  int' (x   : xs) p = int' xs $ p * 10 + digitToInt x

readToList :: IO [Int]
readToList = map (int . T.unpack) . T.words <$> I.getLine

saveMoney :: IORef Int -> IORef Int -> Int -> IO()
saveMoney saved money fut = do
  mn <- readIORef money
  if mn - fut < 100 then
    return ()
  else do
    modifyIORef' saved (+100)
    modifyIORef' money (\x -> x-100)
    saveMoney saved money fut

main :: IO ()
main = do
  pocketMoney <- newIORef (0::Int)
  savedMoney <- newIORef (0::Int)
  for_ ([1..12]::[Int]) $ \ i -> do
    [v] <- readToList
    modifyIORef' pocketMoney (+300)
    mn <- readIORef pocketMoney
    if mn < v then do
      printf "%d" (-i)
      exitSuccess
    else
      saveMoney savedMoney pocketMoney v
    modifyIORef' pocketMoney (\x -> x - v)
  sv <- readIORef savedMoney
  pk <- readIORef pocketMoney
  printf "%.0f" ((fromIntegral sv * 1.2 + fromIntegral pk)::Double)
  return ()
