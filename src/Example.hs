module Main where

import System.IO
import Parser (imp, emptyState)
import AST (factorial, ite, unsatEx)
import Model (generateModel)
import Text.Parsec


main :: IO ()
-- main = do writeFile "factorial.rs" (generateModel factorial)
--           writeFile "ite.rs" (generateModel factorial)
--           writeFile "unsatEx.rs" (generateModel unsatEx)


main = do file <- readFile "examples/factorial.imp"
          case (runParser imp emptyState "examples/factorial.imp" file) of
            Left err -> putStrLn $ show err
            Right ast -> do writeFile "examples/factorial.rs" (generateModel ast)
                            putStrLn $ show (factorial == ast)
          file <- readFile "examples/ite.imp"
          case (runParser imp emptyState "examples/ite.imp" file) of
            Left err -> putStrLn $ show err
            Right ast -> do writeFile "examples/ite.rs" (generateModel ast)
                            putStrLn $ show (ite == ast)
                            
           
          
