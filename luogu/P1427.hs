module P1427 where
import Data.Foldable
import Text.Printf
main :: IO()
main = do
  x <- words <$> getLine 
  let (_:xs) = reverse x
  for_ xs $ \str ->
    printf "%s " str