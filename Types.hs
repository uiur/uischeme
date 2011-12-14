module Types ( Val(..), SchemeError(..), Env, IOThrowsError, ThrowsError, throwError, catchError, trapError, extractValue, getVar, setVar, defineVar, liftThrows, runIOThrows, nullEnv ) where

import Control.Monad.Error
import Text.Parsec (ParseError)
import Data.IORef

type Env = IORef [(String, IORef Val)]
type IOThrowsError = ErrorT SchemeError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError Val
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar :: Env -> String -> Val -> IOThrowsError Val
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar :: Env -> String -> Val -> IOThrowsError Val
defineVar envRef var value = do alreadyDefined <- liftIO $ isBound envRef var
                                if alreadyDefined
                                   then setVar envRef var value >> return value
                                   else liftIO $ do 
                                     valueRef <- newIORef value
                                     env <- readIORef envRef
                                     writeIORef envRef ((var, valueRef) : env)
                                     return value

bindVars :: Env -> [(String, Val)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
        addBinding (var, value) = do ref <- newIORef value
                                     return (var, ref)

nullEnv :: IO Env
nullEnv = newIORef [] 

data Val = Symbol String
         | List [Val]
         | DotList [Val] Val
         | Number Integer
         | String String
         | Bool Bool

data SchemeError = NumArgs Integer [Val]
                 | TypeMismatch String Val
                 | Parser ParseError
                 | BadSpecialForm String Val
                 | NotFunction String String
                 | UnboundVar String String
                 | Default String

showError :: SchemeError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

unwordsList :: [Val] -> String
unwordsList = unwords . map show

instance Show SchemeError where show = showError

instance Show Val where
  show (Symbol xs) = xs
  show (List xs) = "(" ++ (unwords $ map show xs) ++ ")"
  show (DotList xs x) = "(" ++ (unwords $ map show xs) ++ " . " ++ (show x) ++ ")"
  show (Number n) = show n
  show (String xs) = "\"" ++ xs ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"

instance Error SchemeError where
  noMsg = Default "An error has occured!"
  strMsg = Default

type ThrowsError = Either SchemeError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

