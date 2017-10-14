module Parser(Aexp(..), Bexp(..), Stm(..), AST, parseProgram) where

import Combinators
import Control.Applicative
import Data.Char

--------------------------------------
-- ASTs
type AST = Stm

data Aexp = Num Integer
          | Ident String
          | StringLit String
          | Add Aexp Aexp
          | Mult Aexp Aexp
          | Div Aexp Aexp
          | Sub Aexp Aexp
          | ReadInt -- TODO does not belong, move once things are exprs
          deriving Show

data Bexp = Val Bool
          | Eq Aexp Aexp
          | Lteq Aexp Aexp
          | Neg Bexp
          | And Bexp Bexp
            deriving Show

-- TODO make most (or all) things expression
data Stm = Assn String Aexp
          | Skip
          | Comp Stm Stm
          | If Bexp Stm Stm
          | While Bexp Stm
          | Try Stm Stm
          | Print Aexp
          deriving Show

--------------------------------------
-- Parsing and IO
main = do ast <- fmap parseProgram getContents
          case ast of
            (Left err) -> putStrLn err
            (Right ast) -> putStrLn (show ast)

parseProgram :: String -> Either String AST
parseProgram inp = case (parse stm inp) of
                 [] -> Left "Unable to parse"
                 [(output, remainder)] -> 
                   if all isSpace remainder
                      then Right output
                      else Left ("Parsing failed. Unparsed Input:\n" ++ remainder)

--------------------------------------
-- Arithmetic Expressions

aexp :: Parser Aexp
aexp = do lhs <- term
          do symbol "+"
             rhs <- aexp
             return (Add lhs rhs)
            <|> do symbol "-"
                   rhs <- aexp
                   return (Sub lhs rhs)
            <|> return lhs

term :: Parser Aexp
term = do lhs <- factor
          do symbol "*"
             rhs <- term
             return (Mult lhs rhs)
            <|> do symbol "/"
                   rhs <- term
                   return (Div lhs rhs)
            <|> return lhs

factor :: Parser Aexp
factor = do symbol "("
            expr <- aexp
            symbol ")"
            return expr
          <|> do symbol "read_int"
                 return ReadInt
          <|> do n <- integer
                 return (Num n)
          <|> do id <- identifier
                 return (Ident id)
          <|> do s <- stringLiteral 
                 return (StringLit s)

--------------------------------------
-- Boolean Expressions

bexp :: Parser Bexp
bexp = do p <- predicate
          return p
        <|> do a1 <- aexp
               symbol "="
               a2 <- aexp
               return (Eq a1 a2)
        <|> do a1 <- aexp
               symbol "<="
               a2 <- aexp
               return (Lteq a1 a2)

predicate :: Parser Bexp
predicate = do lhs <- clause
               do and <- symbol "&&"
                  rhs <- predicate
                  return (And lhs rhs)
                <|> return lhs

clause :: Parser Bexp
clause = do symbol "("
            b <- bexp
            symbol ")"
            return b
         <|> do t <- symbol "!"
                b <- bexp
                return (Neg b)
         <|> do t <- symbol "true"
                return (Val True)
         <|> do t <- symbol "false"
                return (Val False)

--------------------------------------
-- Statements

stm :: Parser Stm
stm = do many comment
         s1 <- stmParen
         do many comment
            newline 
            many comment
            s2 <- stm
            many comment
            return (Comp s1 s2)
          <|> return s1

stmParen :: Parser Stm
stmParen = do symbol "("
              s <- stm
              symbol ")"
              return s
            <|> statement

statement :: Parser Stm
statement  = do id <- identifier
                symbol ":="
                a <- aexp
                return (Assn id a)
             <|> do symbol "try"
                    s1 <- stm
                    symbol "catch"
                    s2 <- stm
                    return (Try s1 s2)
             <|> do symbol "skip"
                    return Skip
             <|> do symbol "if"
                    b <- bexp
                    symbol "then"
                    s1 <- stm
                    symbol "else"
                    s2 <- stm
                    return (If b s1 s2)
             <|> do symbol "while"
                    b <- bexp
                    s <- stm
                    return (While b s)
             <|> do symbol "print "
                    str <- aexp
                    return (Print str)
