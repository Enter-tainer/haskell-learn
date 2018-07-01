module SimpleTokenizer (Token(..), tokenize) where
import Data.List
import Data.Char
data Token = Token String | Brackets [Token]
  deriving (Eq, Show)

tokenize x
  | isValidBracketsSeq x == False = Nothing
  | otherwise = Just $ mtokenize $ trim x

mtokenize :: String -> [Token]
mtokenize str@(x:xs) 
  | x == '(' = (Brackets $ mtokenize $ realPre) 
               : (mtokenize $ trim suc)
  | otherwise = let (now, rem) = span (\y -> isSame x y) str
                in (Token $ trim now) : (mtokenize $ trim rem)
  where cnt qwq = length . filter (==qwq)
        pre = head $ tail $ filter (\x -> (cnt '(' x) == (cnt ')' x)) $ inits str
        suc = drop (length pre) str
        realPre = init $ tail $ trim pre
mtokenize [] = []

isValidBracketsSeq :: String -> Bool
isValidBracketsSeq raw_str = leftCnt raw_str == rightCnt raw_str &&
  (allTrue $ map (\x -> leftCnt x >= rightCnt x) $ inits raw_str)
  where leftCnt = length . filter (=='(')
        rightCnt = length . filter (==')')
        allTrue (x:xs) = x && allTrue xs
        allTrue [] = True

isSame :: Char -> Char -> Bool
isSame x y = (isOp x && isOp y) || (isLetter x && isLetter y)
  where isOp qwq = qwq `elem` "!#$%&*+-/<=>@^_.,;"

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace