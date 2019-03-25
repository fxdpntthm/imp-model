{-# LANGUAGE BlockArguments #-}
module ParserSpec (spec) where

import AST
import Parser (imp, emptyState)

import Text.Parsec

import Test.Hspec

-- Write a full fledged factorial program
-- Y := X;
-- Z := 1;
-- while 1<Y do
--   Z := Z * Y;
--   Y := Y - 1;
-- Y := 0
l1 :: Com
l1 = Assign "Y" (Var "X") 1

l2 :: Com
l2 = Assign "Z" (ANat 1) 2

l3 :: Com
l3 = While (Leq (ANat 1) (Var "Y")) (Seq l4 l5) 3

l4 :: Com
l4 = Assign "Z" (Mult (Var "Z") (Var "Y")) 4

l5 :: Com
l5 = Assign "Y" (Minus (Var "Y") (ANat 1)) 5

l6 :: Com
l6 = Assign "Y" (ANat 0) 6


-- Factorial will be given as an input to the get-model stuff
factorial :: Com
factorial = Seq l1 (Seq l2 (Seq l3 l6))

-- Another example using if else
{-
Y := X;
IF (1 <= Z) THEN (Z := Y) ELSE (Z := 10);
Y := 0;
-}
l1' :: Com
l1' = Assign "Y" (Var "X") 1

l2' :: Com
l2' = IfThenElse (Leq (ANat 1) (Var "Z"))
                 (Assign "Z" (Var "Y") 3)
                 (Assign "Z" (ANat 10) 4) 2

l3' :: Com
l3' = Assign "Y" (ANat 0) 5

iteAst :: Com
iteAst = Seq l1' (Seq l2' l3')


-- One more example
{-
WHILE (X <= 0) DO (X := X + 1);
SKIP
-}
unsatAst :: Com
unsatAst = Seq (While (Leq (Var "X") (ANat 0))
                (Assign "X" (Plus (Var "X") (ANat 1)) 2) 1) (Skip 3)

spec :: Spec
spec =
  describe "Parsing Tests" $ do
    describe "parsing factorial.imp" $ do
      it "returns the correct AST" $ do
        file <- readFile "examples/factorial.imp"
        case (runParser imp emptyState "examples/factorial.imp" file) of
          Left err -> error $ show err 
          Right ast -> ast `shouldBe` factorial
      
    describe "parsing ite.imp" $ do
      it "returns the correct AST" $ do
        file <- readFile "examples/ite.imp"
        case (runParser imp emptyState "examples/ite.imp" file) of
          Left err -> error $ show err
          Right ast -> ast `shouldBe` iteAst

    describe "parsing unsat.imp" $ do
      it "returns the correct AST" $ do
        file <- readFile "examples/unsat.imp"
        case (runParser imp emptyState "examples/unsat.imp" file) of
          Left err -> error $ show err
          Right ast -> ast `shouldBe` unsatAst

