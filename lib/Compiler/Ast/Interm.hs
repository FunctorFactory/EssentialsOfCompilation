-- |
-- Module      : Compiler.Ast.Lisp
-- Description : Types to represent the grammar of our intermediate language
-- Copyright   : (c) Functor Factory
-- License     : GPL-3.0 or later
--
-- Maintainer  : jshilling@functorfactory.com
-- Stability   : experimental
-- Portability : portable
--
-- This is the syntax tree for the intermediate, C-like language that our input
-- lanaguage is first translated into during the explicit control phase.
--
-- atm  ::= int | var
-- exp  ::= atm | (- atm) | (+ atm atm) | (- atm atm)
-- stmt ::= var = exp;
-- tail ::= return exp; | stmt tail
--
-- program  ::= (label: tail)...
module Compiler.Ast.Interm
  ( Atom (..),
    Expression (..),
    Statement (..),
    Tail (..),
    Program (..),
  )
where

import Data.List (intercalate)

data Atom
  = IntLiteral Int
  | Var String

data Expression
  = Atom Atom
  | Neg Atom
  | Add Atom Atom
  | Sub Atom Atom

data Statement = Assign String Expression

data Tail
  = Return Expression
  | Seq Statement Tail

newtype Program
  = Program [(String, Tail)]

instance Show Atom where
  show (IntLiteral a) = show a
  show (Var a) = a

instance Show Expression where
  show (Atom a) = show a
  show (Neg a) = "(- " ++ show a ++ ")"
  show (Add a b) = "(+ " ++ show a ++ " " ++ show b ++ ")"
  show (Sub a b) = "(- " ++ show a ++ " " ++ show b ++ ")"

instance Show Statement where
  show (Assign var e) = var ++ " = " ++ show e ++ ";"

instance Show Tail where
  show (Return e) = "return " ++ show e ++ ";"
  show (Seq s t) = show s ++ "\n" ++ show t

instance Show Program where
  show (Program []) = ""
  show (Program l) =
    let toString :: (String, Tail) -> String
        toString (label, body) =
          let tailStrs = map ("  " ++) $ lines $ show body
              label' = label ++ ":"
           in intercalate "\n" (label' : tailStrs)
     in intercalate "\n" (map toString l)
