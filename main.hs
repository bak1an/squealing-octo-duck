module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Data.Complex
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
             | Complex (Complex Integer)
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
        val <- try (string "space" <|> string "newline") <|> count 1 anyChar
        return $ Character $ case val of
                     "space" -> ' '
                     "newline" -> '\n'
                     _ -> val !! 0

parseComplex :: Parser LispVal
parseComplex = do
        real <- many digit
        char '+'
        imag <- many digit
        char 'i'
        return $ Complex (read real :+ read imag)

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
parseExpr = parseAtom
         <|> parseChar
         <|> parseString
         <|> try parseComplex
         <|> try parseFloat
         <|> try parseNumber
         <|> try parseBool
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
        head <- endBy parseExpr spaces
        tail <- char '.' >> spaces >> parseExpr
        return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
        x <- char '\'' >> parseExpr
        return $ List [Atom "quote", x]

showVal :: LispVal -> String
showVal (String s) = "\"" ++ s ++ "\""
showVal (Complex v) = "Complex(" ++ show v ++ ")"
showVal (Number n) = show n
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Float f) = show f
showVal (Character c) = "'" ++ [c] ++ "'"
showVal (Atom a) = "Atom(" ++ a ++ ")"
showVal (List l) = "(" ++ (unwords . map showVal $ l) ++ ")"
showVal (DottedList h t) = "(" ++ (unwords . map showVal $ h) ++ " . " ++ (showVal t) ++ ")"

instance Show LispVal where
        show = showVal

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op args = Number $ foldl1 op $ map unpackNum args

unpackNum :: LispVal -> Integer
unpackNum (Number x) = x
unpackNum _ = 0

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod)]

apply :: String -> [LispVal] -> LispVal
apply f args = maybe (Bool False) ($ args) $ lookup f primitives

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Float _) = val
eval val@(Character _) = val
eval val@(Complex _) = val
eval val@(Atom _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom f : args)) = apply f $ map eval args

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

main :: IO ()
main = getArgs >>= print . eval . readExpr . head

