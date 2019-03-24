module Parser where

import Control.Monad
import Data.Char (digitToInt, isUpper, isLower, isOctDigit, isHexDigit, ord)
import Data.Either (partitionEithers)
import Data.List (partition)
import qualified Data.Map as Map
import Data.Maybe (isJust, fromMaybe)
import Text.Parsec
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Lexer



import           AST ( Id
                     , LNo
                     , AExp (ANat, Plus, Minus, Mult, Var)
                     , BExp (BTrue, BFalse, Leq, IsZero, Or, Not, And)
                     , Com (Skip, While, IfThenElse, Assign, Seq)
                     , factorial )


emptyState :: Int
emptyState = 1

aVarid  :: ParseM Id           -- basic form of variable name
aVarid   = identifier isUpper

variable :: ParseM AExp
variable = do i <- aVarid
              return (Var i)

nat :: ParseM AExp -- parses a natural number
nat = do n <- (natural <?> "natural number")
         return (ANat (fromIntegral n))

aOperators :: [[Operator Char Int AExp]]
aOperators = [ [Infix  (reservedOp "*"   >> return (Mult ))     AssocLeft]
             , [Infix  (reservedOp "+"   >> return (Plus     )) AssocLeft,
                Infix  (reservedOp "-"   >> return (Minus))     AssocLeft]]

arithTerm :: ParseM AExp
arithTerm =  parens arithExp
             <|> variable
             <|> nat

arithExp :: ParseM AExp
arithExp = buildExpressionParser aOperators arithTerm  

bOperators :: [[Operator Char Int BExp]]
bOperators = [[Prefix (reservedOp "!" >> return (Not             ))          ]
             , [Infix  (reservedOp "&&" >> return (And     )) AssocLeft,
                Infix  (reservedOp "||"  >> return (Or      )) AssocLeft] 
             ]

boolTerm :: ParseM BExp
boolTerm = parens boolExp
           <|>(reserved "TRUE"  >> return (BTrue))
           <|> (reserved "FALSE" >> return (BFalse))
           <|> rExp

rExp :: ParseM BExp 
rExp = do a1 <- arithTerm
          reservedOp "<="
          a2 <- arithTerm
          return $ Leq a1 a2

boolExp :: ParseM BExp
boolExp = buildExpressionParser bOperators boolTerm


assign :: ParseM Com  -- parses and assignment
assign = do v <- aVarid
            reservedOp ":="
            e <- arithExp
            lno <- getState
            modifyState (\x -> x + 1)
            return (Assign v e lno) 

skip :: ParseM Com
skip = do reserved "SKIP"
          lno <- getState
          modifyState (\x -> x + 1)
          return (Skip lno)


while :: ParseM Com
while = do reserved "WHILE"
           b <- boolExp
           reserved "DO"
           lno <- getState
           modifyState (\x -> x + 1)
           c <- command
           return (While b c lno)

ite :: ParseM Com
ite = do reserved "IF"
         lno <- getState
         modifyState (\x -> x + 1)
         b <- boolExp
         reserved "THEN"
         c1 <- command
         reserved "ELSE"
         c2 <- command
         return (IfThenElse b c1 c2 lno)

se :: ParseM Com
se = do l <- (sepBy1 command' semi)
        return $ seqHelper l
        where
          seqHelper [] = Skip 0
          seqHelper [c1] = c1
          seqHelper (c1: c2: []) = Seq c1 c2
          seqHelper (c1:c2:ys) = Seq c1 (Seq c2 (seqHelper ys)) 

command' :: ParseM Com
command' = skip 
          <|> assign
          <|> while
          <|> ite

command :: ParseM Com
command = parens command
  <|> se

-- | Parses the imp language
imp :: ParseM Com
imp = whiteSpace >> command
