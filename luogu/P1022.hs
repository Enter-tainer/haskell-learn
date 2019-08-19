module P1022 where

import           Data.Char
import           Text.Printf

data Result = Result Double Char Double deriving (Show)-- Double x + Double = 0 kx + b = 0

solve :: String -> Result
solve str = solve' str 0 0 0 'G' 1 1

digitToDb :: Char -> Double
digitToDb = fromIntegral . digitToInt

solve'
  :: String -> Double -> Double -> Double -> Char -> Double -> Double -> Result
solve' [] b parsingVal k unk isLeft isNeg =
  Result k unk (b + isNeg * isLeft * parsingVal)
solve' (x : xs) b parsingVal k unk isLeft isNeg
  | x == '+'  = solve' xs (b + isNeg * isLeft * parsingVal) 0 k unk isLeft 1
  | x == '-'  = solve' xs (b + isNeg * isLeft * parsingVal) 0 k unk isLeft (-1)
  | x == '='  = solve' xs (b + isNeg * isLeft * parsingVal) 0 k unk (-1) 1
  | isDigit x = solve' xs b (parsingVal * 10 + digitToDb x) k unk isLeft isNeg
  | isAlpha x && parsingVal == 0 = solve' xs b 0 (isLeft * isNeg) x isLeft isNeg
  | otherwise = solve' xs b 0 (k + isNeg * isLeft * parsingVal) x isLeft isNeg

main :: IO ()
main = do
  str <- filter (not . isSpace) <$> getLine
  let (Result k x b) = solve str
  printf "%c=%.3f" x $ pocc (-b/k)
  return ()
  where
    pocc x = if (isNegativeZero x) then 0 else x
