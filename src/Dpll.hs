{-
  Implement the bare minimal DPLL algorithm.
  The algorithm includes two techniques:
    1. Pure literal elimination.
    2. Unit propagation.
-}

module Dpll where
import           BtBase
import           Cnf
import           LiteralElimination
import           UnitPropagation


satDpll :: Expr -> Bool
satDpll expr =
  let outputRes res = case res of
        Const b -> b
        _       -> error "Must output result from Const."
      goodExpr = litElimination (cnf (unitProp expr))
  in  case findFree goodExpr of
        Nothing -> outputRes (simplify goodExpr)
        Just v ->
          let guessTrue  = simplify (guessVar v True goodExpr)
              guessFalse = simplify (guessVar v False goodExpr)
          in  satDpll guessTrue || satDpll guessFalse


formula = And
  (And
    (And
      (And
        (And
          (And (And (Or (Var 'a') (Var 'b')) (Or (Var 'c') (Var 'd')))
               (Or (Var 'e') (Var 'f'))
          )
          (Not (Var 'b'))
        )
        (Not (Var 'c'))
      )
      (Const True)
    )
    (Or
      (Or (And (Var 'a') (And (Not (Var 'c')) (Not (Var 'e'))))
          (And (Var 'c') (And (Not (Var 'a')) (Not (Var 'e'))))
      )
      (And (Var 'e') (And (Not (Var 'a')) (Not (Var 'c'))))
    )
  )
  (Or
    (Or (And (Var 'b') (And (Not (Var 'd')) (Not (Var 'f'))))
        (And (Var 'd') (And (Not (Var 'b')) (Not (Var 'f'))))
    )
    (And (Var 'f') (And (Not (Var 'b')) (Not (Var 'd'))))
  )
