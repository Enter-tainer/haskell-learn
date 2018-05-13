module LongestCommonSubsequence where

lcs :: String -> String -> String
lcs x y
    | null x || null y = []
    | last x == last y = (lcs (init x) (init y)) ++ [(last x)]
    | otherwise = if length l < length r
                  then r
                  else l
        where l = lcs (init x) y
              r = lcs x $ init y