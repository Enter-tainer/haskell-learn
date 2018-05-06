{-# OPTIONS_GHC -optc-O3 #-}
module LastDigit (lastDigit) where

fastPow :: Integer -> Integer -> Integer -> Integer
fastPow base 1 m = mod base m
fastPow base 0 m = 1
fastPow base pow m | even pow = mod ((fastPow base (div pow 2) m) ^ 2) m
                    | odd  pow = mod ((fastPow base (div (pow-1) 2) m) ^ 2 * base) m

lastDigit :: [Integer] -> Integer
lastDigit [] = 1
lastDigit xs = (foldr1 (\x acc -> fastPow x acc 9982443531926081723333333333000) xs) `mod` 10