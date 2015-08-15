module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric (readHex, readOct, readFloat, readDec)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

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
             | Character Char
             | Float Float

parseBool :: Parser LispVal
parseBool = char '#' >> oneOf "tf" >>= \v -> return $ case v of
                                                          't' -> Bool True
                                                          'f' -> Bool False


parseChar :: Parser LispVal
parseChar = do
        try $ string "#\\"
        first <- anyChar
        rest <- many $ noneOf " "
        let res = if length rest == 0 then first
                                      else case first:rest of
                                               "space" -> ' '
                                               "newline" -> '\n'
        return $ Character res

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
        return $ Atom atom

parseFloat :: Parser LispVal
parseFloat = do
    f <- many1 digit
    char '.'
    s <- many1 digit
    return $ Float . fst . (!! 0) . readFloat $ (f ++ ['.'] ++ s)

parseNumberDec :: Parser LispVal
parseNumberDec = liftM (Number . read) $ many1 digit

parseNumberRad :: Parser LispVal
parseNumberRad = do
        prefix <- char '#' >> oneOf "dxo"
        val <- many1 $ case prefix of
                           'd' -> digit
                           'x' -> hexDigit
                           'o' -> octDigit
        let r = case prefix of
                    'd' -> readDec
                    'x' -> readHex
                    'o' -> readOct
        return $ Number . fst . (!! 0) . r $ val

parseNumber = parseNumberDec <|> parseNumberRad

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseChar <|> parseString <|> try parseFloat <|> try parseNumber <|> try parseBool

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

main :: IO ()
main = do
    expr:_ <- getArgs
    putStrLn (readExpr expr)



