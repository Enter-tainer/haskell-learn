{-# OPTIONS_GHC -O2 #-}
-- {-# language Strict #-}
module Main where
import Data.Char (digitToInt, isSpace)
import Text.Printf (printf)
import qualified Data.Text as T
import qualified Data.Text.IO as I

repM :: Monad m => Int -> a -> (a -> m a) -> m a
repM 0 x _ = return x
repM n x f = f x >>= \y -> repM (n - 1) y f

repM_ :: Monad m => Int -> a -> (a -> m a) -> m ()
repM_ n x f = repM n x f >> return ()

int :: String -> Int
int str = int' (filter (not . isSpace) str) 0
  where
    int' [] x = x
    int' ('-':xs) _ = -1 * (int' xs 0)
    int' (x:xs) p = int' xs $ p * 10 + digitToInt x

data ParingTree a = ParingTree a [ParingTree a]
data ParingHeap a = Empty | ParingHeap (ParingTree a)

val :: (Ord a) => ParingTree a -> a
val (ParingTree v _) = v

subHeaps :: (Ord a) => ParingTree a -> [ParingTree a]
subHeaps (ParingTree _ t) = t

findMin :: (Ord a) => ParingHeap a -> a
findMin Empty = error "Cannot access an empty heap"
findMin (ParingHeap t) = val t

infixl 5 ><

(><) :: (Ord a) => ParingHeap a -> ParingHeap a -> ParingHeap a
Empty >< v = v
u >< Empty = u
l@(ParingHeap u) >< r@(ParingHeap v)
  | val u < val v = ParingHeap (ParingTree (val u) (v:(subHeaps u)))
  | otherwise = r >< l

infixl 5 <|
(<|) :: (Ord a) => ParingHeap a -> a -> ParingHeap a
tr <| val = tr >< ParingHeap (ParingTree val [])

deleteMin :: (Ord a) => ParingHeap a -> ParingHeap a
deleteMin Empty = error "Cannot delete anything from empty heap"
deleteMin (ParingHeap (ParingTree _ prs)) = mergePairs prs

mergePairs :: (Ord a) => [ParingTree a] -> ParingHeap a
mergePairs [] = Empty
mergePairs [t] = ParingHeap t
mergePairs (x:y:xs) = (ParingHeap x >< ParingHeap y) >< (mergePairs xs)

main :: IO ()
main = do
  n <- int <$> T.unpack <$> I.getLine
  repM_ n (Empty) $ \(root) -> do
    (x:xs) <- map (int . T.unpack) . T.words <$> I.getLine
    case x of 1 -> do
                     let n:[] = xs
                     return (root <| n)
              2 -> printf "%d\n" (findMin root) >> return (root)
              3 -> return (deleteMin root)
