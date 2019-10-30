{- Unit Propagation -}

module UnitPropagation where
import           Data.Maybe                     ( mapMaybe )
import           BtBase


-- Determine if a clause is a unit clause.
isUnitClause :: Expr -> Maybe (Char, Bool)
isUnitClause clause = case clause of
  (Var c      ) -> Just (c, True)
  (Not (Var c)) -> Just (c, False)
  _             -> Nothing


-- Convert CNF to a list of clauses.
toClauses :: Expr -> [Expr]
toClauses expr = case expr of
  (And a b) -> toClauses a ++ toClauses b
  _         -> [expr]


-- Do unit propagation on an Expr.
unitProp :: Expr -> Expr
unitProp expr =
  let getAllUnits  = mapMaybe isUnitClause . toClauses -- Expr -> [(Char, Bool)]
      unitPairings = getAllUnits expr -- [(Char, Bool)]
      replacers    = map (uncurry guessVar) unitPairings -- [Expr -> Expr]
      replaceAll   = foldl (.) id replacers -- Expr -> Expr
  in  replaceAll expr
