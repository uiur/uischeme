module Eval where

import Types
import Parse

primitives :: [(String, [Val] -> Val)]
primitives = [("+", numberBinop (+)),
              ("*", numberBinop (*)),
              ("/", numberBinop div),
              ("-", numberBinop (-))]

numberBinop :: (Integer -> Integer -> Integer) -> [Val] -> Val
numberBinop op = Number . (foldl1 op) . mapNumber

numberToInteger :: Val -> Integer
numberToInteger (Number n) = n

mapNumber :: [Val] -> [Integer]
mapNumber = map (\(Number n) -> n)

apply :: String -> [Val] -> Val
apply func args = maybe (Bool False) ($ args) $ lookup func primitives 

eval :: Val -> Val
eval val@(Number _) = val
eval val@(String _) = val
eval val@(Bool _)   = val
eval (List ((Symbol x):args)) = apply x $ map eval args

re :: String -> Val
re = eval . readExpr
