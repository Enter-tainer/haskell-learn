module MiddlePermutation.JorgeVS.Kata where
import Data.Int
import Data.List

middlePermutation :: String -> String
middlePermutation myString  = getkthPermutation (sort myString) (n - 1)
  where
    n = floor (((fromIntegral $ fact $ fromIntegral $ length myString) / 2)::Double)

fact :: Int64 -> Int64
fact 1 = 1
fact n = n * fact (n - 1)

getkthPermutation :: String -> Int64 -> String
getkthPermutation [] _ = []
getkthPermutation str 0 = str
getkthPermutation str k = (str !! idx) : getkthPermutation xs (k `mod` fact (l - 1))
  where
    l = fromIntegral $ length str
    idx = fromIntegral $ div k $ fact (l - 1)
    xs = sort $ str \\ [str !! idx]
