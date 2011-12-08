module Types where

data Val = Symbol String
         | List [Val]
         | DottedList [Val] Val
         | Number Integer
         | String String
         | Bool Bool

instance Show Val where
  show (Symbol xs) = xs
  show (List xs) = "(" ++ (unwords $ map show xs) ++ ")"
  show (DottedList xs x) = "(" ++ (unwords $ map show xs) ++ " . " ++ (show x) ++ ")"
  show (Number n) = show n
  show (String xs) = "\"" ++ xs ++ "\""
  show (Bool True) = "#t"
  show (Bool False) = "#f"


