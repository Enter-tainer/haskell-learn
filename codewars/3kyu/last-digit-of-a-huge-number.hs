module LastDigit (lastDigit) where
lastDigit :: [Integer] -> Integer
lastDigit xs = (foldr (\x acc -> x ^ (if acc < 4 then acc else acc `mod` 4 + 4)) 1  xs) `mod` 10