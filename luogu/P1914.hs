module P1914 where

import Data.Char

succC :: Char -> Char
succC 'z' = 'a'
succC x = chr $ ord x + 1

shift :: String -> Int -> String
shift str 0 = str
shift str n = shift xs $ n - 1
  where
    xs = map succC str

main :: IO ()
main = do
  n <- (read <$> getLine):: IO Int
  str <- filter (not . isSpace) <$> getLine
  putStrLn $ shift str n
  return ()
