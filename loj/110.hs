import Data.List

inv :: Int -> Int -> Int
inv n p = invVec !! n
  where
    invVec = gen (n + 1) f
    gen len f' = map f' $ take len [0..]
    f 1 = 1
    f k = ((invVec !! (p `mod` k)) * (p - p `div` k) `mod` p) `mod` p

main :: IO()
main = do
  str <- getLine
  let [n, p] = map read $ words str
  let ans = map (flip inv p) [1..n]
  putStrLn $ intercalate "\n" $ map show ans
  return ()