module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric (readHex, readOct)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

escapedChars :: Parser Char
escapedChars = do
        char '\\'
        x <- oneOf "\"\\nrt"
        return $ case x of
                   '"' -> x
                   '\\' -> x
                   'n' -> '\n'
                   't' -> '\t'
                   'r' -> '\r'

spaces :: Parser ()
spaces = skipMany1 space


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

parseString :: Parser LispVal
parseString = do
        char '"'
        x <- many $ escapedChars <|> noneOf "\"\\"
        char '"'
        return $ String x

parseAtom :: Parser LispVal
parseAtom = do
        first <- letter <|> symbol
        rest <- many (letter <|> digit <|> symbol)
        let atom = first : rest
        return $ case atom of
                     "#t" -> Bool True
                     "#f" -> Bool False
                     _    -> Atom atom

parseNumberDec :: Parser LispVal
parseNumberDec = liftM (Number . read) $ many1 digit

parseNumberRad :: Parser LispVal
parseNumberRad = do
        prefix <- char '#' >> oneOf "dxo"
        val <- many1 digit
        let r = case prefix of
                    'd' -> read
                    'x' -> readHex
                    'o' -> readOct
        return $ Number . read $ val

parseNumber = parseNumberDec <|> parseNumberRad

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO ()
main = do
    expr:_ <- getArgs
    putStrLn (readExpr expr)



