-- |
-- Module      : Compiler.Ast.Lisp
-- Description : Types to represent the grammar of our input language
-- Copyright   : (c) Functor Factory
-- License     : GPL-3.0 or later
--
-- Maintainer  : jshilling@functorfactory.com
-- Stability   : experimental
-- Portability : portable
--
-- Here we layout the types that will represent our grammar for the compiler's
-- input language. This language definition will be extended each chapter and Siek
-- treats each chapter as introducing a new language that is a superset of the one
-- from previous chapters.
{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}
module Compiler.Ast.Lisp
  ( Program(..),
    Expression (..),
  )
where
import GHC.Generics (Generic)

newtype Program = Program Expression

-- |
-- exp ::= int
--       | var
--       | (- exp)
--       | (+ exp exp)
--       | (- exp exp)
--       | (let ([var exp]) exp)
-- program :: = exp
data Expression where
  IntLiteral :: Int -> Expression
  Var :: String -> Expression
  Neg :: Expression -> Expression
  Add :: Expression -> Expression -> Expression
  Sub :: Expression -> Expression -> Expression
  Let :: String -> Expression -> Expression -> Expression
  deriving Generic

instance Show Expression where
  show (IntLiteral a) = show a
  show (Var a) = a
  show (Neg a) = "(- " ++ show a ++ ")"
  show (Add a b) = "(+ " ++ show a ++ " " ++ show b ++ ")"
  show (Sub a b) = "(- " ++ show a ++ " " ++ show b ++ ")"
  show (Let var val body) =
    let preamble = "(let ([" ++ var ++ " "
        valLines = lines $ show val
        valStr = unlines' $ indentRest (length preamble) valLines
     in unlines'
          [ preamble ++ valStr ++ "])",
            "  " ++ show body ++ ")"
          ]

unlines' :: [String] -> String
unlines' [] = ""
unlines' l = begin ++ end
  where
    begin = unlines . init $ l
    end = last l

indent :: Int -> [String] -> [String]
indent n = map (spaces ++)
  where
    spaces = replicate n ' '

indentRest :: Int -> [String] -> [String]
indentRest _ [] = []
indentRest n (x : xs) = x : rest
  where
    rest = indent n xs
