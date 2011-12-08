module Eval where

import Types
import Parse

primitives :: [(String, [Val] -> Val)]
primitives = [("+", numberBinop (+)),
              ("*", numberBinop (*)),
              ("/", numberBinop div),
              ("-", numberBinop (-)),
              ("=", boolBinop (==)),
              ("<", boolBinop (<)),
              ("<=", boolBinop (<=)),
              (">", boolBinop (>)),
              (">=", boolBinop (>=))
              ]
              

boolBinop :: (Integer -> Integer -> Bool) -> [Val] -> Val
boolBinop p xs = Bool $ and $ zipWith p nums $ tail nums
  where
    nums = map (\(Number n) -> n) xs

numberBinop :: (Integer -> Integer -> Integer) -> [Val] -> Val
numberBinop op = Number . (foldl1 op) . mapNumber

mapNumber :: [Val] -> [Integer]
mapNumber = map (\(Number n) -> n)

valToBool :: Val -> Bool
valToBool (Bool False) = False
valToBool _ = True

apply :: String -> [Val] -> Val
apply func args = maybe (Bool False) ($ args) $ lookup func primitives 

eval :: Val -> Val
eval val@(Number _) = val
eval val@(String _) = val
eval val@(Bool _)   = val
eval (List [(Symbol "if"), cond, thenVal, elseVal]) = eval $ if t then thenVal else elseVal
                        where 
                          t = valToBool $ eval cond

eval (List ((Symbol x):args)) = apply x $ map eval args

re :: String -> Val
re = eval . readExpr
