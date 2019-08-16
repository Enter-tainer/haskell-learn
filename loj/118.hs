module L118 where
import Text.Regex.Posix
import Data.Char

mzip :: [String] -> [(String, String)]
mzip [] = []
mzip (x:y:xs) = (x, y) : (mzip xs)

mmatch :: (String, String) -> Bool
mmatch (reg, str) = str =~ reg

mprint :: [Bool] -> IO()
mprint [] = return ()
mprint (x:xs) = do
  if x then do
    putStrLn "Yes"
  else do
    putStrLn "No"
  mprint xs
main :: IO()
main = do 
  contents <- getContents
  let res = mzip $ words $ contents
  let arr = map mmatch res
  mprint arr
  return ()