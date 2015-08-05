module Main where

import System.Environment

main :: IO ()
main = do
    args <- getArgs
    let a = read $ args !! 0 :: Integer
        b = read $ args !! 1 :: Integer
    putStrLn $ "sum is " ++ (show $ a + b)
    putStr "now give me your name: "
    name <- getLine
    putStrLn $ "hello " ++ name

