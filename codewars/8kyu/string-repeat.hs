module StringRepeat where

repeatStr :: Int -> String -> String
repeatStr 0 str = ""
repeatStr 1 str = str
repeatStr n str = str ++ repeatStr (n - 1) str