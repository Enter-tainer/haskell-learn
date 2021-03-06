module SimpleTokenizer (Token(..), tokenize) where
import Data.List
import Data.Char
data Token = Token String | Brackets [Token]
  deriving (Eq, Show)
tokenize :: String -> Maybe [Token]
tokenize x
  | isValidBracketsSeq x == False = Nothing
  | otherwise = Just $ mtokenize $ x

mtokenize :: String -> [Token]
mtokenize str@(x:xs)
  | x == ' ' = mtokenize xs
  | x == '(' = (Brackets $ mtokenize $ realPre) 
               : (mtokenize suc)
  | otherwise = let (now, remain) = span (\y -> isSame x y) str
                in (Token now) : (mtokenize remain)
  where cnt qwq = length . filter (==qwq)
        _:pre:_ = filter (\xqwq -> (cnt '(' xqwq) == (cnt ')' xqwq)) $ inits str
        suc = drop (length pre) str
        realPre = init $ tail pre
mtokenize [] = []

isValidBracketsSeq :: String -> Bool
isValidBracketsSeq rawStr = leftCnt rawStr == rightCnt rawStr &&
  (allTrue $ map (\x -> leftCnt x >= rightCnt x) $ inits rawStr)
  where leftCnt = length . filter (=='(')
        rightCnt = length . filter (==')')
        allTrue (x:xs) = x && allTrue xs
        allTrue [] = True

isSame :: Char -> Char -> Bool
isSame x y = (isOp x && isOp y) || (isLetter x && isLetter y)
  where isOp qwq = qwq `elem` "!#$%&*+-/<=>@^_.,;"