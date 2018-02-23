module Difference where

mydiff :: Eq a => [a] -> a -> [a]
mydiff alist dif = [qwq | qwq <- alist, qwq /= dif]

difference :: Eq a => [a] -> [a] -> [a]
difference a [] = a
difference [] _ = []
difference a (x:xs) = difference (mydiff a x) xs