import Data.Int
import Data.List

inv :: Int64 -> Int64 -> Int64
inv n p = invVec !! intN
  where
    intN = fromIntegral n
    invVec = gen (intN + 1) f
    gen len f' = map f' $ take len [0..]
    f 1 = 1
    f k = ((invVec !! fromIntegral (p `mod` k)) * (p - p `div` k) `mod` p) `mod` p

main :: IO()
main = do
  str <- getLine
  let [n, p] = map read $ words str
  let ans = map (flip inv p) [1..n]
  putStrLn $ intercalate "\n" $ map show ans
  return ()