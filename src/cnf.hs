{-
  Defines transformation methods towards DNF.
  Two techniques are included:
    1. Keep applying De Morgan's law to make sure negations are only on literals.
    2. Distribute disjunctions over conjunctions.
-}

module Cnf where
import           BtBase


-- Fix negations by: (1) removing double negations; (2) applying De Morgan's.
fixNegations :: Expr -> Expr
fixNegations expr = case expr of
  -- Remove double negation
  Not (Not e  ) -> fixNegations e
  -- De Morgan's
  Not (And a b) -> Or (fixNegations (Not a)) (fixNegations (Not b))
  Not (Or  a b) -> And (fixNegations (Not a)) (fixNegations (Not b))
  -- Constants
  Not (Const b) -> Const (not b)
  -- subterms
  Not e         -> Not (fixNegations e)
  And a b       -> And (fixNegations a) (fixNegations b)
  Or  a b       -> Or (fixNegations a) (fixNegations b)
  e             -> e


-- Distribute disjunction over conjunction.
distribute :: Expr -> Expr
distribute expr = case expr of
  Or x (And a b) ->
    And (Or (distribute x) (distribute a)) (Or (distribute x) (distribute b))
  Or (And a b) x ->
    And (Or (distribute x) (distribute a)) (Or (distribute x) (distribute b))
  -- subterms
  Or  a b -> Or (distribute a) (distribute b)
  And a b -> And (distribute a) (distribute b)
  Not e   -> Not (distribute e)
  e       -> e


-- Convert an Expr to CNF.
cnf :: Expr -> Expr
cnf expr =
  let newExpr = distribute (fixNegations expr)
  in  if newExpr == expr then expr else cnf newExpr
