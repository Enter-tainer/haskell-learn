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

data Heap a = Empty | Heap a (Heap a) (Heap a)

val :: (Ord a) => Heap a -> a
val (Heap v _ _ ) = v

findMin :: (Ord a) => Heap a -> a
findMin Empty = error "Cannot access an empty heap"
findMin (Heap t _ _) = t

infixl 5 ><

(><) :: (Ord a) => Heap a -> Heap a -> Heap a
Empty >< v = v
u >< Empty = u
u@(Heap uval ulc urc) >< v@(Heap vval vlc vrc)
  | uval < vval = Heap uval (urc >< v) (ulc)
  | otherwise = v >< u

infixl 5 <|
(<|) :: (Ord a) => Heap a -> a -> Heap a
tr <| val = tr >< Heap val Empty Empty

deleteMin :: (Ord a) => Heap a -> Heap a
deleteMin Empty = error "Cannot delete anything from empty heap"
deleteMin (Heap _ lc rc) = lc >< rc


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
              _ -> error "nmsl"
