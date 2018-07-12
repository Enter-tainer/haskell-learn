{-# LANGUAGE FlexibleInstances #-}
module ConvexHull.Kata (hull) where
import Prelude hiding ((<*>))
import Data.List
infixl 7 <*>
infixl 7 <^>
type Point = (Double,Double)
type Vector = Point
instance Num Point where
  (x, y) + (x', y') = (x + x', y + y')
  (x, y) * (x', y') = (x * x', y * y')
  (x, y) - (x', y') = (x - x', y - y')
  abs (x, y) = (abs x, abs y)
(<*>) :: Vector -> Vector -> Double
(x, y) <*> (x', y') = x * x' + y * y'
(<^>) :: Vector -> Vector -> Double
(x, y) <^> (x', y') = x * y' - x' * y
isHigher :: Point -> Point -> Point -> Bool
isHigher (x1, y1) (x2, y2) (x, y)
  | y1 == y2 = False
  | otherwise = y > (f x)
  where
    f xx = y1 + (xx - x1) * (y2 - y1) / (x2 - x1)
vecLength :: Vector -> Double
vecLength (x, y) = sqrt (x * x + y * y)
theta :: Vector -> Vector -> Double
theta a@(x, y) b@(x', y') = acos ((a <*> b) / ((vecLength a) * (vecLength b))) 
getX :: (a, b) -> a
getX = fst
getY :: (a, b) -> b
getY = snd
-- theta :: Vector -> Vector -> Double
-- theta a b = acos ()

hull :: [Point] -> [Point]
hull x
  | length x <= 3 = x
  | otherwise = mergeHull leftHull rightHull
    where 
      pointSet = map head $ group $ sort x
      pointNumber = length pointSet
      leftPoints = reverse $ take (pointNumber `div` 2) pointSet
      rightPoints = drop (pointNumber `div` 2) pointSet
      leftHull = hull leftPoints
      rightHull = hull rightPoints
      mergeHull l r = undefined
      --   where
          
      
      
