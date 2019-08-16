module P1008 where
import           Data.List
import           Data.Foldable
import           Text.Printf
check :: [Int] -> Bool
check xs = "123456789" == sort (concat (show <$> xs))

solve :: [(Int, Int, Int)]
solve = [(a, 2 * a, 3 * a) | a <- [123..329]::[Int], check [a, 2 * a, 3 * a]]

main :: IO()
main = for_ solve $ \(a, b, c) -> printf "%d %d %d\n" a b c

