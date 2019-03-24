module Model where

import           Data.List (nub)

import           AST -- ( Id
                     -- , LNo
                     -- , AExp (ANat, Plus, Minus, Mult, Var)
                     -- , BExp (BTrue, BFalse, Leq, IsZero, Or, Not)
                     -- , Com (Skip, While, IfThenElse, Assign, Seq)
                     -- , vars
                     -- , factorial )


-- | converts a list of ids to spaced strings
-- listIds :: [Id] -> String
-- listIds [] = " "
-- listIds (x:xs) = x ++ " " ++ listIds xs

-- | takes the name of z3 datatype, the various values it can take
-- | returns the z3 datatype declaration
declareDataTypes :: String -> [Id] -> String
declareDataTypes dname ids =
  "(declare-datatypes () ((" ++ dname ++ (foldl (\x y -> x ++ " " ++ y) "" ids) ++ ")))\n" 


generateComment :: String -> String
generateComment comment = ";; " ++ comment ++ "\n"

findLastLabel :: Com -> Int
findLastLabel (Skip lno) = lno
findLastLabel (While _ com _) = findLastLabel com
findLastLabel (IfThenElse _ _ _ lno) = lno
findLastLabel (Assign _ _ lno) = lno
findLastLabel (Seq _ com) = findLastLabel com

findFirstLabel :: Com -> Int
findFirstLabel (Skip lno) = lno
findFirstLabel (While _ _ lno) = lno 
findFirstLabel (IfThenElse _ _ _ lno) = lno
findFirstLabel (Assign _ _ lno) = lno
findFirstLabel (Seq com _) = findFirstLabel com

data PStage = Entry | Exit

-- | This function accepts a command and returns string of assertions
generateAssertions :: PStage -> Com  -> Com -> String

generateAssertions Entry (Assign _ _ 1) _ = "" 
generateAssertions Entry (Skip 1) _ = "" 
generateAssertions Entry (While _ _ 1) _ = "" 
generateAssertions Entry (IfThenElse _ _ _ 1) _ = "" 


-- for skip the entry is exit of previous
generateAssertions Entry c@(Skip lno) c' =
  generateComment "Entry for "
  ++ generateComment (show c)
  ++ generateComment "is exit of prev com"
  ++ "(assert (forall ((v VAR) (l LAB)) (= (en n"
  ++ show lno ++ " v l) (ex n" ++ show (findFirstLabel c') ++" v l))))\n\n"


-- Assignment entry is same as previous exit
generateAssertions Entry c@(Assign _ _ lno) c' =
  generateComment "Entry for "
  ++ generateComment (show c)
  ++ generateComment "is exit of previous label"
  ++ "(assert (forall ((v VAR) (l LAB)) (= (en n"
  ++ show lno ++ " v l) (ex n" ++ show (findFirstLabel c') ++" v l))))\n\n"

-- for While entry is union of previous exit and exit of last com (need to inspect com)
generateAssertions Entry c@(While _ com lno) c' =
  generateComment "Entry for "
  ++ generateComment (show c)
  ++ generateComment "is union of exit of previous line no and exit of last command"
  ++ "(assert (forall ((v VAR) (l LAB))"
  ++ "(= (or (ex n" ++ show lno' ++ " v l) (ex n" ++ show (findFirstLabel c') ++ " v l))"
  ++ "(en n" ++ show lno ++ " v l))))\n\n"
  ++ generateAssertions Entry com c
  ++ generateAssertions Exit com com

  where lno' = findLastLabel com

-- for If entry is same as exit
generateAssertions Entry c@(IfThenElse _ com1 com2 lno) c' =
  generateComment "Entry for "
  ++ generateComment (show c)
  ++ generateComment "is same as exit of previous line"
  ++ "(assert (forall ((v VAR) (l LAB)) (= (en n"
  ++ show lno ++ " v l) (ex n" ++ show (findLastLabel c') ++" v l))))\n\n"
  ++ generateAssertions Entry com1 c'
  ++ generateAssertions Exit com1 com1
  ++ generateAssertions Entry com2 c'
  ++ generateAssertions Exit com2 com2



-- Sequencing should trigger assertion check for internal commands  
generateAssertions Entry (Seq com1 com2) c' =
  generateAssertions Entry com1 c'
  ++ generateAssertions Exit com1 com1
  ++ generateAssertions Entry com2 com1
  ++ generateAssertions Exit com2 com2

-- Exit of skip is same as entry of skip
generateAssertions Exit c@(Skip lno) _ =
  generateComment "Exit for"
  ++ generateComment (show c)
  ++ generateComment" does nothing"
  ++ "(assert (forall ((v VAR) (l LAB)) (= (ex n" ++ show lno ++ " v l) (en n" ++ show lno ++" v l))))\n\n"

-- Exit of while is same as entry of while
generateAssertions Exit c@(While _ _ lno) _ =
  generateComment "Exit for "
  ++ generateComment (show c)
  ++ generateComment " is same as entry of while"
  ++ "(assert (forall ((v VAR) (l LAB)) (= (ex n" ++ show lno ++ " v l) (en n" ++ show lno ++" v l))))\n\n"

-- exit of if then else is union of exit of both branches  
generateAssertions Exit c@(IfThenElse _ com com' lno) _ =
  generateComment "Exit for "
  ++ generateComment (show c)
  ++ generateComment " is union of exits of both the commands"
  ++ "(assert (forall ((v VAR) (l LAB))"
  ++ "(= (or (ex n" ++ show lno1' ++ " v l) (ex n" ++ show lno2' ++ " v l))"
  ++ "(ex n" ++ show lno ++ " v l))))\n\n"
  where lno1' = findLastLabel com
        lno2' = findLastLabel com'

-- For assignment, remove all previous lables and add the current label
generateAssertions Exit c@(Assign i _ lno) _ =
  generateComment "Exit for"
  ++ generateComment (show c)
  ++ generateComment "is by removing all previous labels and keeping this label"
  ++ "(assert (forall ((v VAR) (l LAB)) "
  ++ "(ite (= v " ++ i ++ ") "
  ++ "(ite (= l l"++ show lno ++ ") "
  ++ "(ex n" ++ show lno ++ " v l)"
  ++ "(not (ex n" ++ show lno ++ " v l)))"
  ++ "(= (ex n" ++  show lno ++ " v l) (en n" ++ show lno ++ " v l)))))\n\n"


generateAssertions Exit (Seq _ _) _= ""


-- | Generates the datatype and function declarations
-- | the first argument is the list of Variables in the program
-- | The second argument is the max label number
generatePrefixDecls :: [Id] -> Int -> String
generatePrefixDecls ids no =
  (generateComment "Datatype declarations")
  ++ (declareDataTypes "VAR" ids)
  ++ (declareDataTypes "LAB" (["l?"] ++ take no (["l" ++ (show n) | n <- [1..]])))
  ++ (declareDataTypes "LNO" (take no (["n" ++ (show n) | n <- [1..]]))) ++ "\n"
  ++ (generateComment "entry and exit states as functions")
  ++ "(declare-fun en (LNO VAR LAB) Bool)\n"
  ++ "(declare-fun ex (LNO VAR LAB) Bool)\n"
  ++ "\n\n" ++ generateComment "Assertions about the states"
  ++ generateComment "Entry state for n1 is all hooks"
  ++ "(assert (forall ((v VAR) (l LAB)) (ite (= l l?) (en n1 v l) (not (en n1 v l)))))\n\n"
  ++ generateComment "entry and exit for next follows"


getModel :: String
getModel = generateComment "check for satisfiability and generate model"
  ++ "(check-sat)\n(get-model)\n"

generateModel :: Com -> String
generateModel program =
  generatePrefixDecls (nub $ vars program) (findLastLabel program)
  ++ generateAssertions Entry program program ++ "\n"
  ++ getModel
