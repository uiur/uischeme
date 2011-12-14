module Eval where

import Types
import Parse
import Control.Monad

primitives :: [(String, [Val] -> ThrowsError Val)]
primitives = [("+", numberBinop (+)),
              ("*", numberBinop (*)),
              ("/", numberBinop div),
              ("-", numberBinop (-)),
              ("=", boolBinop (==)),
              ("<", boolBinop (<)),
              ("<=", boolBinop (<=)),
              (">", boolBinop (>)),
              (">=", boolBinop (>=)),
              ("cons", cons),
              ("car", car),
              ("cdr", cdr)
              ]
              

boolBinop :: (Integer -> Integer -> Bool) -> [Val] -> ThrowsError Val
boolBinop p xs = return $ Bool $ and $ zipWith p nums $ tail nums
  where
    nums = mapNumber xs

numberBinop :: (Integer -> Integer -> Integer) -> [Val] -> ThrowsError Val
numberBinop op = return . Number . (foldl1 op) . mapNumber

mapNumber :: [Val] -> [Integer]
mapNumber = map (\(Number n) -> n)

valToBool :: Val -> Bool
valToBool (Bool False) = False
valToBool _ = True

apply :: String -> [Val] -> ThrowsError Val
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func) ($ args) $ lookup func primitives 

eval :: Env -> Val -> IOThrowsError Val
eval _ val@(Number _) = return val
eval _ val@(String _) = return val
eval _ val@(Bool _)   = return val
eval env (Symbol var) = getVar env var
eval _ (List [(Symbol "quote"), x]) = return x
eval env (List [(Symbol "if"), cond, thenVal, elseVal]) = 
                            do result <- eval env cond
                               case result of
                                 Bool True -> eval env thenVal
                                 _         -> eval env elseVal 
eval env (List [Symbol "set!", Symbol var, form]) = eval env form >>= setVar env var
eval env (List [Symbol "define", Symbol var, form]) = eval env form >>= defineVar env var 
eval env (List ((Symbol f):args)) = mapM (eval env) args >>= liftThrows . apply f
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm 

-- car, cdr, cons
car  :: [Val] -> ThrowsError Val
car [List (x:xs)] = return x
car [DotList (x:xs) _] = return x
car [badArg]   = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr  :: [Val] -> ThrowsError Val
cdr [List (x:xs)] = return $ List xs
cdr [DotList [x] y] = return y
cdr [DotList (x:xs) y] = return $ DotList xs y
cdr [badArg]  = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [Val] -> ThrowsError Val
cons [x, (List y)]= return $ List (x:y)
cons [x, (DotList y z)] = return $ DotList (x:y) z
cons [x, y] = return $ DotList [x] y
cons badArgList = throwError $ NumArgs 2 badArgList


