module Calculator where
import Data.List
data Operator = OPlus | OSub | OMul | ODiv deriving (Eq, Show)

data Expr =
    Lit Double
  | Mul Expr Expr
  | Plus Expr Expr
  | Div Expr Expr
  | Sub Expr Expr
  deriving (Eq, Show)
data Token =
  Number Double
  | Op Operator
  deriving (Eq, Show)

eval :: Expr -> Double
eval (Lit x) = x
eval (Mul a b) = eval a * eval b
eval (Plus a b) = eval a + eval b
eval (Div a b) = eval a / eval b
eval (Sub a b) = eval a - eval b

isOp :: Char -> Bool
isOp = (flip elem) "+-*/"

toToken :: String -> Token
toToken "+" = Op OPlus
toToken "-" = Op OSub
toToken "*" = Op OMul
toToken "/" = Op ODiv
toToken str = Number $ read str

tokenize :: String -> [Token]
tokenize l = toToken <$> splited
  where
    splited = words l

priority :: Token -> Int
priority (Number _) = 3
priority x
  | x `elem` [Op OPlus, Op OSub] = 1
  | x `elem` [Op OMul, Op ODiv] = 2
  | otherwise = 3

findLastHighPriority :: [Token] -> Int
findLastHighPriority xs
  | (minimum $ pl) == 1 = last $ findIndices (==1) pl
  | (minimum $ pl) == 2 = last $ findIndices (==2) pl
  | otherwise = 0
  where
    pl = priority <$> xs

breakAt :: Int -> [a] -> ([a], [a])
breakAt x xs = (a, tail b)
  where
    (a, b) = splitAt x xs

parse :: [Token] -> Expr
parse [(Number t)] = Lit t
parse xs = case v of
  OPlus -> Plus ll rr
  OSub -> Sub ll rr
  ODiv -> Div ll rr
  OMul -> Mul ll rr
  where
    (l, r) = breakAt idx xs
    [ll, rr] = parse <$> [l, r]
    (Op v) = xs !! idx
    idx = findLastHighPriority xs

evaluate :: String -> Double
evaluate = eval . parse . tokenize
