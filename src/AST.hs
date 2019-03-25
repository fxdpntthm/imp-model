module AST where

-- Labels
type LNo = Int

type Id = String 

-- Specification of the abstract syntax tree
-- aexp ::= Nat | aexp + aexp | aexp - aexp | Var
data AExp = ANat Int
          | Plus AExp AExp
          | Minus AExp AExp
          | Mult AExp AExp
          | Var Id
          deriving (Show, Eq)

-- bexp ::= aexp <= aexp | isZero aexp | bexp or bexp | not bexp
data BExp = BTrue
          | BFalse
          | IsZero AExp
          | Leq AExp AExp
          | Or BExp BExp
          | And BExp BExp
          | Not BExp
          deriving (Show, Eq)

-- com ::= skip | while bexp do com | if bexp then com else com | Var := aexp | com ; com
data Com = Skip LNo
         | While BExp Com LNo
         | IfThenElse BExp Com Com LNo
         | Assign Id AExp LNo
         | Seq Com Com
          deriving (Show, Eq)
class Variable a where
  vars :: a -> [Id]


instance Variable Com where
  vars (While ex com _) = vars com ++ vars ex
  vars (IfThenElse ex com com' _) = vars com ++ vars com' ++ vars ex
  vars (Assign i ex _) = [i] ++ vars ex
  vars (Seq com com') = vars com ++ vars com'
  vars _ = []

instance Variable AExp where
  vars (Var i) = [i] 
  vars (Plus ex ex') = vars ex ++ vars ex'
  vars (Minus ex ex') = vars ex ++ vars ex'
  vars (Mult ex ex') = vars ex ++ vars ex'
  vars _ = []

instance Variable BExp where
  vars (Leq ex ex') = vars ex ++ vars ex'
  vars (IsZero ex) = vars ex
  vars (Or ex ex') = vars ex ++ vars ex'
  vars (Not ex) = vars ex
  vars _ = []


-- Write a full fledged factorial program
-- Y := X;
-- Z := 1;
-- while 1<Y do
--   Z := Z * Y;
--   Y := Y - 1;
-- Y := 0
-- l1 :: Com
-- l1 = Assign "Y" (Var "X") 1

-- l2 :: Com
-- l2 = Assign "Z" (ANat 1) 2

-- l3 :: Com
-- l3 = While (Leq (ANat 1) (Var "Y")) (Seq l4 l5) 3

-- l4 :: Com
-- l4 = Assign "Z" (Mult (Var "Z") (Var "Y")) 4

-- l5 :: Com
-- l5 = Assign "Y" (Minus (Var "Y") (ANat 1)) 5

-- l6 :: Com
-- l6 = Assign "Y" (ANat 0) 6


-- Factorial will be given as an input to the get-model stuff
-- factorial :: Com
-- factorial = Seq l1 (Seq l2 (Seq l3 l6))

-- Another example using if else
{-
Y := X;
IF (1 <= Z) THEN (Z := Y) ELSE (Z := 10);
Y := 0;
-}
-- l1' :: Com
-- l1' = Assign "Y" (Var "X") 1

-- l2' :: Com
-- l2' = IfThenElse (Leq (ANat 1) (Var "Z"))
--                  (Assign "Z" (Var "Y") 3)
--                  (Assign "Z" (ANat 10) 4) 2

-- l3' :: Com
-- l3' = Assign "Y" (ANat 0) 5

-- ite :: Com
-- ite = Seq l1' (Seq l2' l3')


-- One more example
{-
WHILE (X <= 0) DO (X := X + 1);
SKIP
-}
-- unsatEx :: Com
-- unsatEx = Seq (While (Leq (Var "X") (ANat 0))
--                 (Assign "X" (Plus (Var "X") (ANat 1)) 2) 1) (Skip 3)
