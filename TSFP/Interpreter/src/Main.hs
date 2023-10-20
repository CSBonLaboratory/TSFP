module Main where

import Syntax.Grammar (parseProgram)

main :: IO ()
main = readFile "mainprog.txt"
     >>= maybe (putStrLn "Syntax error!") (mapM_ print) . parseProgram