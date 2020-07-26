{-# OPTIONS_GHC -O2 #-}

-- {-# language Strict #-}
module P3378LH where

import Data.Char (digitToInt, isSpace)
import qualified Data.Text as T
import qualified Data.Text.IO as I
import Text.Printf (printf)

repM :: Monad m => Int -> a -> (a -> m a) -> m a
repM 0 x _ = return x
repM n x f = f x >>= \y -> repM (n - 1) y f

repM_ :: Monad m => Int -> a -> (a -> m a) -> m ()
repM_ n x f = repM n x f >> return ()

int :: String -> Int
int str = int' (filter (not . isSpace) str) 0
  where
    int' [] x = x
    int' ('-' : xs) _ = -1 * (int' xs 0)
    int' (x : xs) p = int' xs $ p * 10 + digitToInt x

data Heap a = Empty | Heap a Int (Heap a) (Heap a)

dis :: Heap a -> Int
dis Empty = 0
dis (Heap _ v _ _) = v

val :: (Ord a) => Heap a -> a
val Empty = error "empty heap has no value"
val (Heap v _ _ _) = v

findMin :: (Ord a) => Heap a -> a
findMin Empty = error "Cannot access an empty heap"
findMin (Heap t _ _ _) = t

infixl 5 ><

(><) :: (Ord a) => Heap a -> Heap a -> Heap a
Empty >< v = v
u >< Empty = u
u@(Heap uval udis ulc urc) >< v@(Heap vval vdis vlc vrc)
  | uval <= vval =
    let x = urc >< v
     in if dis ulc < dis x
          then Heap uval (dis ulc + 1) x ulc
          else Heap uval (dis x + 1) ulc x
  | otherwise = v >< u

infixl 5 <|

(<|) :: (Ord a) => Heap a -> a -> Heap a
tr <| val = tr >< Heap val 1 Empty Empty

deleteMin :: (Ord a) => Heap a -> Heap a
deleteMin Empty = error "Cannot delete anything from empty heap"
deleteMin (Heap _ _ lc rc) = lc >< rc

main :: IO ()
main = do
  n <- int <$> T.unpack <$> I.getLine
  repM_ n (Empty) $ \(root) -> do
    (x : xs) <- map (int . T.unpack) . T.words <$> I.getLine
    case x of
      1 -> do
        let num : [] = xs
        return (root <| num)
      2 -> printf "%d\n" (findMin root) >> return (root)
      3 -> return (deleteMin root)
      _ -> error "nmsl"
