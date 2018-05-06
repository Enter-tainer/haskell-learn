module Haskell.SylarDoom.MazeRunner where

mazeRunner :: [[Int]] -> String -> String
mazeRunner maze dir
    | "Dead" `elem` ans = "Dead"
    | "Finish" `elem` ans = "Finish"
    | otherwise = "Lost"
        where ans = moveall maze (findval maze 2) dir

moveall :: [[Int]] -> (Int, Int) -> String -> [String]
moveall maze (x, y) dirs
    | null dirs = []
    | not $ inmaze maze (x', y') = ["Dead"]
    | otherwise =
        case place of 1 -> ["Dead"]
                      3 -> ["Finish"]
                      _ -> "Normal" : otherdir
        where (x', y') = move maze (x, y) (head dirs)
              otherdir = moveall maze (x', y') (tail dirs)
              place = getval maze (x', y')
              inmaze maze (xx, yy) = xx > 0 && yy > 0 && xx <= l && yy <= l
                  where l = length maze

move :: [[Int]] -> (Int, Int) -> Char -> (Int, Int)
move maze (x, y) dir
    | dir == 'N' = (x - 1, y)
    | dir == 'S' = (x + 1, y)
    | dir == 'W' = (x, y - 1)
    | dir == 'E' = (x, y + 1)

getval :: [[Int]] -> (Int, Int) -> Int
getval maze (x, y) = head $ drop (y - 1) (head $ drop (x - 1) maze)

findval :: [[Int]] -> Int -> (Int, Int)
findval maze val = (x + 1, y + 1)
    where x = length $ takeWhile (notelem val) maze
          y = length $ takeWhile (/= val) (head $ dropWhile (notelem val) maze)

notelem :: (Eq a) => a -> [a] -> Bool
notelem x xs = not $ x `elem` xs