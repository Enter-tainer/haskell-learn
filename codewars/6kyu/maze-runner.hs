module Haskell.SylarDoom.MazeRunner where

mazeRunner :: [[Int]] -> String -> String
mazeRunner maze dir = if "Dead" `elem` ans then "Dead" else if "Finish" `elem` ans then "Finish" else "Lost"
    where ans = moveall maze (findval maze 2) dir

moveall :: [[Int]] -> (Int, Int) -> String -> [String]
moveall maze (x, y) dirs
    | null dirs = []
    | otherwise = if place == "1" then ("Dead": otherdir) else if place == "3" then ("Finish": otherdir) else ("Normal":otherdir)
        where (x', y') = move maze (x, y) (head dirs)
              otherdir = moveall maze (x', y') (tail dirs)
              place = show $ getval maze (x', y')
move :: [[Int]] -> (Int, Int) -> Char -> (Int, Int)
move maze (x, y) dir
    | dir == 'N' = (x - 1, y)
    | dir == 'S' = (x + 1, y)
    | dir == 'W' = (x, y - 1)
    | dir == 'E' = (x, y + 1)

getval :: [[Int]] -> (Int, Int) -> Int
getval maze (x, y) = head $ drop (y - 1) (drop (x - 1) maze)

findval :: [[Int]] -> Int -> (Int, Int)
findval maze val = (x + 1, y + 1)
    where x = length $ takeWhile (not $ elem val) maze
          y = length $ takeWhile (/= val) (head $ dropWhile (not $ elem val) maze)