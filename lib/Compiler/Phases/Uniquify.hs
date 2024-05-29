-- |
-- Module      : Compiler.Phases.Uniquify
-- Description : Types to represent the grammar of our input language
-- Copyright   : (c) Functor Factory
-- License     : GPL-3.0 or later
--
-- Maintainer  : jshilling@functorfactory.com
-- Stability   : experimental
-- Portability : portable
--
-- This phases takes the freshly parsed AST as input and replaces all variable
-- identifiers with ones that are globally unique.
module Compiler.Phases.Uniquify
  ( uniquify
  )
where

import Data.Map.Strict (Map)
import Control.Monad.Trans.State.Strict (State, get, put, evalState)
import qualified Data.Map.Strict as Map
import qualified Compiler.Ast.Lisp as Lisp

type IdentifierMap = Map String Int

uniquify :: Lisp.Program -> Lisp.Program
uniquify (Lisp.Program e) = Lisp.Program $ evalState (uniquifyExpression e) Map.empty

getNextIdentifier :: String -> State IdentifierMap String
getNextIdentifier s = do
  ids <- get
  let suffix = show $ Map.findWithDefault 0 s ids
  return $ s ++ suffix

updateIdentifierMap :: String -> IdentifierMap -> IdentifierMap
updateIdentifierMap s ids = Map.insert s nextSuffix ids
  where nextSuffix = succ $ Map.findWithDefault 0 s ids

uniquifyBinaryExpression :: (Lisp.Expression -> Lisp.Expression -> Lisp.Expression) -> Lisp.Expression -> Lisp.Expression -> State IdentifierMap Lisp.Expression
uniquifyBinaryExpression f e1 e2 = do
  initialState <- get
  let newE1 = evalState (uniquifyExpression e1) initialState
  let newE2 = evalState (uniquifyExpression e2) initialState
  return (f newE1 newE2)

uniquifyUnaryExpression :: (Lisp.Expression -> Lisp.Expression) -> Lisp.Expression -> State IdentifierMap Lisp.Expression
uniquifyUnaryExpression f e = f <$> uniquifyExpression e

uniquifyExpression :: Lisp.Expression -> State IdentifierMap Lisp.Expression
uniquifyExpression (Lisp.IntLiteral n) = return (Lisp.IntLiteral n)
uniquifyExpression (Lisp.Var var) = Lisp.Var <$> getNextIdentifier var
uniquifyExpression (Lisp.Neg e) = uniquifyUnaryExpression Lisp.Neg e
uniquifyExpression (Lisp.Add e1 e2) = uniquifyBinaryExpression Lisp.Add e1 e2
uniquifyExpression (Lisp.Sub e1 e2) = uniquifyBinaryExpression Lisp.Sub e1 e2
uniquifyExpression (Lisp.Let var e1 e2) = do
  initialState <- get
  let newState = updateIdentifierMap var initialState
  let newVar = evalState (getNextIdentifier var) initialState
  let newE1 = evalState (uniquifyExpression e1) initialState
  let newE2 = evalState (uniquifyExpression e2) newState
  return (Lisp.Let newVar newE1 newE2)
