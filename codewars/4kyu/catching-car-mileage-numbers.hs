{-# LANGUAGE TemplateHaskell #-}
module Awesome.Numbers where
import Data.Char
import Data.List
data Answer = No | Almost | Yes deriving (Show, Read, Eq, Ord)
remove10 :: Int -> Int
remove10 a = case a `mod` 10
  of 0 -> remove10 $ floor ((fromIntegral a) / 10)
     _ -> a

everyDigit :: Int -> [Int]
everyDigit x = map digitToInt $ show x

isFollowedZero :: Int -> Bool
isFollowedZero x = (remove10 x) `elem` [0..9]

isSameNumber :: Int -> Bool
isSameNumber x = maximum arr == minimum arr
  where arr = everyDigit x

isPalindrome :: Int -> Bool
isPalindrome x = arr == reverse arr
  where arr = everyDigit x

isIncreasing' :: [Int] -> Bool
isIncreasing' [] = True
isIncreasing' [_] = True
isIncreasing' (x:xs) =
  case y of
    0 -> length xs == 1 && x == 9
    _ -> y - x == 1 && isIncreasing' xs
  where
    y = head xs

isDecreasing' :: [Int] -> Bool
isDecreasing' [] = True
isDecreasing' [_] = True
isDecreasing' (x:xs) =
  case y of
    0 -> length xs == 1 && x == 1
    _ -> x - y == 1 && isDecreasing' xs
  where
    y = head xs

isIncreasing :: Int -> Bool
isIncreasing = isIncreasing' . everyDigit

isDecreasing :: Int -> Bool
isDecreasing = isDecreasing' . everyDigit

isInteresting' :: Int -> [Int] -> Bool
isInteresting' x xs = x > 99 && (or $ [isIncreasing, isDecreasing, isPalindrome, isFollowedZero, isInList] <*> [x])
  where
    isInList = (flip elem) xs
isInteresting :: Integer -> [Integer] -> Answer
isInteresting x xs
  | isInteresting' x' xs' = Yes
  | isInteresting' (x' + 1) xs' || isInteresting' (x' + 2) xs' = Almost
  | otherwise = No
  where
    x' = fromInteger x
    xs' = fromInteger <$> xs
