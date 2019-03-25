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
