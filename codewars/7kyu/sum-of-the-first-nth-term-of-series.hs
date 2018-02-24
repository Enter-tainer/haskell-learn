module Codewars.Kata.NthSeries where
import Text.Printf
seriesSum x = printf "%.2f" (sum (take (fromIntegral x) [1 / qwq | qwq <- [1,4..]]))