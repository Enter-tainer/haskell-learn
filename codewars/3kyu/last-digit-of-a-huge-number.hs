{-# OPTIONS_GHC -optc-O3 #-}
module LastDigit (lastDigit) where
import qualified Data.Vector as V
lastDigit :: [Integer] -> Integer
lastDigit xs = (V.foldr' (\x acc -> x ^ (if acc < 4 then acc else acc `mod` 4 + 4)) 1 $ V.fromList xs) `mod` 10