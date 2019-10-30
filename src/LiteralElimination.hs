{- Pure literal elimination -}

module LiteralElimination where
import           Data.Maybe                     ( mapMaybe
                                                , catMaybes
                                                )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           BtBase


-- Get all literals in an expression.
allLiterals :: Expr -> Set Char
allLiterals expr = case expr of
  (Var c  ) -> Set.singleton c
  (Not e  ) -> allLiterals e
  (And a b) -> Set.union (allLiterals a) (allLiterals b)
  (Or  a b) -> Set.union (allLiterals a) (allLiterals b)
  _         -> Set.empty


-- Find out the polarity of a literal in an Expr (Expr must be in CNF)
-- Return Nothing if the literal does not appear in the Expr
litPolarity :: Char -> Expr -> Maybe Polarity
litPolarity v e = case e of
  (Var c      ) -> if c == v then Just Positive else Nothing
  (Not (Var c)) -> if c == v then Just Negative else Nothing
  -- sub-expressions
  (And a b    ) -> combinePolarities [a, b]
  (Or  a b    ) -> combinePolarities [a, b]
  Not   expr    -> error ("Not in CNF: negation on non-literal: " ++ show expr)
  Const _       -> Nothing
 where
  combinePolarities es =
    let polarities = mapMaybe (litPolarity v) es
    in  case polarities of
          [] -> Nothing
          ps | all (== Positive) ps -> Just Positive
             | all (== Negative) ps -> Just Negative
             | otherwise            -> Just Mixed


-- Convert a polarity pairing to Bool pairing.
polarityToBool :: Char -> Maybe Polarity -> Maybe (Char, Bool)
polarityToBool c polarity = case polarity of
  (Just Positive) -> Just (c, True)
  (Just Negative) -> Just (c, False)
  _               -> Nothing


-- Do pure literal elimination on an Expr.
litElimination :: Expr -> Expr
litElimination expr =
  let ls          = Set.toList (allLiterals expr)
      ps          = map (`litPolarity` expr) ls
      boolPairing = catMaybes (zipWith polarityToBool ls ps) -- [(Char, Bool)]
      replacers   = map (uncurry guessVar) boolPairing -- [Expr -> Expr]
      replaceAll  = foldl (.) id replacers -- Expr -> Expr
  in  replaceAll expr
