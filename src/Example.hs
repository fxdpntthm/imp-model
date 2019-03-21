module Main where

import AST (factorial, ite)
import Model (generateModel)

main :: IO ()
main = do putStr $ generateModel ite

