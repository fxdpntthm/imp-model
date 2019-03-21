module Main where

import System.IO
import AST (factorial, ite)
import Model (generateModel)

main :: IO ()
main = do writeFile "factorial.rs" (generateModel factorial)
          writeFile "ite.rs" (generateModel factorial)

