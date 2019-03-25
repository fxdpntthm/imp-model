module Main where

import System.IO
import System.Environment
import System.FilePath
import System.Directory
import Parser (imp, emptyState)
import Model (generateModel)
import Text.Parsec


main :: IO ()
main = do [f] <- getArgs
          b <- doesFileExist f
          if b then
            (case (sanitize f) of
               Left err -> error err
               Right f -> do
                 file <- readFile f
                 case (runParser imp emptyState f file) of
                   Left err -> putStrLn $ show err
                   Right ast -> (do writeFile (replaceExtension f "rs") (generateModel ast)
                                    putStrLn $ "z3 output to: " ++ (replaceExtension f "rs")))
            else putStrLn $ "Error: File " ++ f ++ " does not exist"


          
sanitize :: FilePath -> Either String FilePath
sanitize f = if (".imp" /= takeExtension f)
             then Left $ "Error: "++ f ++ " is not a valid Imp file. Imp files should have .imp extension"
             else Right f                     
