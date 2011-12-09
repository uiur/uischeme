module Types ( Val(..), SchemeError(..), ThrowsError, throwError, catchError, trapError, extractValue ) where

import Control.Monad.Error
import Text.Parsec (ParseError)

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

