module P1055 where

import           Data.Char

d2i :: Int -> Char
d2i 10 = 'X'
d2i x  = intToDigit x

getXs :: [a] -> [a]
getXs xs
  | length xs == 9 = xs
  | otherwise = init xs

main :: IO()
main = do
  str <- filter (not . isSpace) <$> getLine
  let xs = getXs $ digitToInt <$> filter isDigit str
  let v = sum (zipWith (*) [1..9] xs) `mod` 11
  if d2i v == last str then
    putStrLn "Right"
  else
    putStrLn (init str ++ [d2i v])
  return ()
