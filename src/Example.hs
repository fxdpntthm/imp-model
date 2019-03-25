module Main where

import System.IO
import Parser (imp, emptyState)
import Model (generateModel)
import Text.Parsec


main :: IO ()
-- main = do writeFile "factorial.rs" (generateModel factorial)
--           writeFile "ite.rs" (generateModel factorial)
--           writeFile "unsatEx.rs" (generateModel unsatEx)


main = do file <- readFile "examples/factorial.imp"
          case (runParser imp emptyState "examples/factorial.imp" file) of
            Left err -> putStrLn $ show err
            Right ast -> writeFile "examples/factorial.rs" (generateModel ast)
          file <- readFile "examples/ite.imp"
          case (runParser imp emptyState "examples/ite.imp" file) of
            Left err -> putStrLn $ show err
            Right ast -> writeFile "examples/ite.rs" (generateModel ast)
                            
           
          
