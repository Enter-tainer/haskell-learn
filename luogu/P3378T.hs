{-# OPTIONS_GHC -O2 #-}
-- {-# language Strict #-}
module P3378T where
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

data Treap a = Node Int a Int (Treap a) (Treap a) | Nil deriving Show -- Size Value Key lc rc

getSize :: Treap a -> Int
getSize Nil = 0
getSize (Node sz _ _ _ _) = sz

split :: (Num a, Ord a) => Treap a -> a -> (Treap a, Treap a)
split Nil _ = (Nil, Nil)
split (Node _ v key lc rc) value
  | v <= value = (Node (getSize l1 + getSize lc + 1) v key lc l1, r1)
  | otherwise = (l2, Node (getSize r2 + getSize rc + 1) v key r2 rc)
    where
      (l1, r1) = split rc value
      (l2, r2) = split lc value

infixl 5 ><

(><) :: (Num a, Ord a) => Treap a -> Treap a -> Treap a
Nil >< u = u
u >< Nil = u
u@(Node _ v1 k1 llc lrc) >< v@(Node _ v2 k2 rlc rrc)
  | k1 <= k2 = Node (getSize rrc + getSize l + 1) v2 k2 l rrc
  | otherwise = Node (getSize llc + getSize r + 1) v1 k1 llc r
  where
    l = u >< rlc
    r = lrc >< v


insert :: (Num a, Ord a) => Treap a -> a -> Int -> Treap a
insert tr v num = l >< t >< r
  where
    (l, r) = split tr v
    t = Node 1 v num Nil Nil

erase :: (Num a, Ord a) => Treap a -> a -> Treap a
erase Nil _ = error "Cannot erase a empty Treap"
erase tr v = ll >< ntr >< r
  where
    (l, r) = split tr v
    (ll, Node _ _ _ rlc rrc) = split l (v - 1)
    ntr = rlc >< rrc

kth :: (Num a, Ord a) => Treap a -> Int -> a
kth Nil _ = error "Cannot kth a empty Treap"
kth (Node _ v _ lc rc) k
  | lsz + 1 == k = v
  | lsz < k = kth rc (k - lsz - 1)
  | otherwise = kth lc k
  where
    lsz = getSize lc

main :: IO ()
main = do
  n <- int <$> T.unpack <$> I.getLine
  repM_ n (Nil, 0) $ \(root, seed) -> do
    (x:xs) <- map (int . T.unpack) . T.words <$> I.getLine
    let res = (1919 * seed * seed + 19260817 * seed + 2333) `mod` 1000000007
    case x of 1 -> do
                     let num:[] = xs
                     return (insert root num res, res)
              2 -> printf "%d\n" (kth root 1) >> return (root, res)
              3 -> return (erase root (kth root 1), res)
              _ -> error "nmsl"
