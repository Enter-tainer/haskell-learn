module P1200 where

import Data.Char

getNum :: String -> Int
getNum xs = product (map (\x -> ord x - ord 'A' + 1) xs) `mod` 47

trim :: String -> String
trim = filter (not . isSpace)

main :: IO ()
main = do
  gp <- trim <$> getLine 
  sc <- trim <$> getLine 
  if getNum gp == getNum sc then
    putStrLn "GO"
  else
    putStrLn "STAY"
  return ()
