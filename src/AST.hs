module AST where

-- Specification of the abstract syntax tree
-- aexp ::= Nat | aexp + aexp | aexp - aexp | Var
data AExp = ANat Int
          | Plus AExp AExp
          | Minus AExp AExp
          | Mult AExp AExp
          | Var String
          deriving (Show)

-- bexp ::= aexp <= aexp | isZero aexp | bexp or bexp | not bexp
data BExp = BTrue
          | BFalse
          | Leq AExp AExp
          | IsZero AExp
          | Or BExp BExp
          | Not BExp
          deriving (Show)

-- com ::= skip | while bexp do com | if bexp then com else com | Var := aexp | com ; com
data Com = Skip
         | While BExp Com
         | IfThenElse BExp Com Com
         | Assign AExp AExp
         | Seq Com Com
          deriving (Show)

-- Some Examples
ex1 :: AExp
ex1 = ANat 3

ex2 :: AExp
ex2 = Plus (ANat 3) (ANat 4)

ex3 :: AExp
ex3 = Minus (ANat 4) (ANat 3)

ex4 :: AExp
ex4 = Var "Y"

ex5 :: BExp
ex5 = Leq (Var "X") ex3

ex6 :: BExp
ex6 = IsZero ex2

ex7 :: BExp
ex7 = Or ex5 ex6

ex8 :: BExp
ex8 = Not ex7

ex9 :: Com
ex9 = Skip

ex10 :: Com
ex10 = While ex5 ex9

ex11 :: Com
ex11 = IfThenElse ex8 ex9 ex10

ex12 :: Com
ex12 = Assign ex4 ex1

ex13 :: Com
ex13 = Seq ex9 ex10

-- Write a full fledged factorial program
-- Y := X;
-- Z := 1;
-- while 1<Y do
--   Z := Z * Y;
--   Y := Y - 1;
-- Y := 0
l1 :: Com
l1 = Assign (Var "Y") (Var "X")

l2 :: Com
l2 = Assign (Var "Z" ) (ANat 1)

l3 :: Com
l3 = While (Leq (ANat 1) (Var "Y")) (Seq l4 l5)

l4 :: Com
l4 = Assign (Var "Z") (Mult (Var "Z") (Var "Y"))

l5 :: Com
l5 = Assign (Var "Y") (Minus (Var "Y") (ANat 1))

l6 :: Com
l6 = Assign (Var "Y") (ANat 0)


-- Factorial will be given as an input to the get-model stuff
factorial :: Com
factorial = Seq l1 (Seq l2 (Seq l3 l6))
