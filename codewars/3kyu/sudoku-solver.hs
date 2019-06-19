module Sudoku where
import Data.List

access :: [[a]] -> (Int, Int) -> a
access p (x, y) = p !! x !! y

replace :: [a] -> Int -> a -> [a]
replace xs ix v = rs l ++ v : (rs r)
  where
    ys = zip [0::Int,1..] xs
    (l, (_:r)) = break (\(idx, _) -> idx == ix) ys
    rs = snd . unzip

takeRange :: [a] -> (Int, Int) -> [a] -- take elements \in [l, r]
takeRange xs (l, r) = take (r - l + 1) $ drop l xs

spanEvery3 :: [a] -> [[a]]
spanEvery3 [a, b, c, u, v, w, i, j, k] = [[a, b, c], [u, v, w], [i, j, k]]
spanEvery3 _ = undefined

modify :: [[a]] -> (Int, Int) -> a -> [[a]]
modify p (x, y) v = replace p x vs
  where
    vs = replace (p !! x) y v

noRepeat :: [Int] -> Bool
noRepeat xs = sort ys == zs
  where
    ys = filter (/=0) xs
    zs = nub $ sort ys

finished :: [Int] -> Bool
finished xs = maximum ys == 9 && minimum ys == 1 && length ys == 9
  where
    ys = nub $ sort xs

check :: ([Int] -> Bool) -> [[Int]] -> Bool
check f p = and $ [(checkLines f), (checkRows f), (checkCells f)] <*> [p]

checkLines :: ([Int] -> Bool) -> [[Int]] -> Bool
checkLines f p = and (f <$> p)

checkRows :: ([Int] -> Bool) -> [[Int]] -> Bool
checkRows f p = and (f <$> (transpose p))

checkCell :: ([Int] -> Bool) -> [[Int]] -> Bool
checkCell f = f . concat

checkCells :: ([Int] -> Bool) -> [[Int]] -> Bool
checkCells ff p = and $ (checkCell ff) <$> (abc ++ uvw ++ ijk)
  where
    x = [0, 1, 2]
    y = [3, 4, 5]
    z = [6, 7, 8]
    sp = spanEvery3 <$> p
    f t xs = [access sp (xx, t) | xx <- xs]
    abc = [f t x| t <- x]
    uvw = [f t y| t <- x]
    ijk = [f t z| t <- x]

haveZero :: [Int] -> Bool
haveZero xs = not $ null $ findIndices (==0) xs

fillFirstZero :: [Int] -> Int -> [Int]
fillFirstZero xs v = case res of
                     Just idx -> replace xs idx v
                     Nothing  -> error "No more zero"
  where
    res = findIndex (==0) xs

sudoku :: [[Int]] -> [[Int]]
sudoku p
  | or xs == False = p
  | ans == [] = [[]]
  | otherwise = head ans
  where
    toBeChk = [res idx v | v <- [1..9]]
    xs = haveZero <$> p
    (Just idx) = findIndex (==True) xs
    res ix v = replace p ix $ fillFirstZero (p !! ix) v
    chked = filter (check noRepeat) toBeChk
    ans = filter (\p -> p /= [[]] && check finished p) (sudoku <$> chked)
