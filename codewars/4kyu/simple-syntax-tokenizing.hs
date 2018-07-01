module SimpleTokenizer (Token(..), tokenize) where
import Data.List
import Data.Char
data Token = Token String | Brackets [Token]
  deriving (Eq, Show)

tokenize x
  | isValidBracketsSeq x == False = Nothing
  | otherwise = Just $ mtokenize $ x

mtokenize :: String -> [Token]
mtokenize str@(x:xs) 
  | x == '(' = (Brackets $ mtokenize $ realPre) 
               : (mtokenize $ trimStart suc)
  | otherwise = let (now, rem) = span (\y -> isSame x y) str
                in (Token $ trimEnd now) : (mtokenize $ trimStart rem)
  where cnt qwq = length . filter (==qwq)
        _:pre:_ = filter (\x -> (cnt '(' x) == (cnt ')' x)) $ inits str
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

trim :: String -> String
trim = trimStart . trimEnd

trimStart :: String -> String
trimStart = dropWhile isSpace

trimEnd :: String -> String
trimEnd = reverse . dropWhile isSpace . reverse