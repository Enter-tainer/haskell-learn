module Kata.GetToTheChoppa where
import qualified Data.Set as Set
import Data.Maybe

type Pos = (Int, Int)

data Node
  = Passable
  | NotPassable
  deriving (Eq, Show)

type Grid = [[Node]]

type Path = [Pos]

data PNode = PNode Pos Int deriving (Show, Eq)
instance Ord PNode where
  compare (PNode _ a) (PNode _ b) = compare a b

mdistance :: Pos -> Pos -> Int
mdistance (x, y) (i, j) = abs (x - i) + abs (y - j)

neighbors :: Grid -> Pos -> [Pos]
neighbors gd (x, y) = [(x', y') | x' <- [x + 1, x - 1], y' <- [y - 1, y + 1], mdistance (x', y') (x, y) == 1, gd !! x' !! y' == Passable]

makePNode :: Pos -> Pos -> Pos -> PNode
makePNode st ed x = PNode x (mdistance st x + mdistance ed x)

aStar :: Grid -> Pos -> Pos -> Set.Set PNode -> Path -> Path
aStar gd st ed pq path@(x:xs)
  | pt == ed = pt:path
  | otherwise = aStar gd st ed newPq (pt:path)
  where
    ((PNode pt _), s) = Set.deleteFindMin pq
    newPq = Set.union (Set.fromList (makePNode st ed <$> neighbors gd pt)) s

shortestPath :: Grid -> Pos -> Pos -> Path
shortestPath gd st ed = reverse $ aStar gd st ed Set.empty []
