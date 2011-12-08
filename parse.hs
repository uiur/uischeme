module Parse where

import Types
import Text.Parsec hiding (spaces)
import Text.Parsec.String
import Control.Monad

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&*+-/:<=>?@^_~"

parseSymbol :: Parser Val
parseSymbol = do first <- letter <|> symbol
                 rest <- many (letter <|> symbol <|> digit)
                 let result = (first:rest)
                 return $ case result of
                               "#t" -> Bool True
                               "#f" -> Bool False
                               _    -> Symbol result

parseList :: Parser Val
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser Val
parseDottedList = do head <- endBy parseExpr spaces
                     tail <- char '.' >> spaces >> parseExpr
                     return $ DottedList head tail

parseNumber :: Parser Val
parseNumber = liftM (Number . read) $ many1 digit

escapedChars :: Parser Char
escapedChars = do char '\\'
                  x <- oneOf "\\\""
                  return x

parseString :: Parser Val
parseString = do char '"'
                 xs <- many $ escapedChars <|> noneOf "\"\\" 
                 char '"'
                 return $ String xs

parseExpr :: Parser Val
parseExpr = parseSymbol
        <|> parseString
        <|> parseNumber
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

readExpr :: String -> Val
readExpr input = case parse parseExpr "(ui)" input of
                      Left err -> String $ "No match: " ++ show err
                      Right val -> val
