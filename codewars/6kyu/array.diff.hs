module Difference where

mydiff :: Eq a => [a] -> a -> [a]
mydiff alist dif = [qwq | qwq <- alist, qwq /= dif]

difference :: Eq a => [a] -> [a] -> [a]
difference a [] = a
difference [] _ = []
difference a (x:xs) = difference (mydiff a x) xs

-- A better version 
difference a b = filter (`notElem` b) a

-- A better version I write
difference a b = [qwq | qwq <- a, qwq `notElem` b]