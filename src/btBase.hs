{-
  Simple SAT solver using backtracking algorithm.
  The algorithm is as follow:
    1. Find a variable in the constraint expression that hasn't been
       assigned (a free variable).
    2. Guess a value for this free variable.
    3. Replace all occurrences of the free variable with the guessed value.
    4. Simplify the expression. If the expression simplifies to true (one),
       then the values we've assigned work, and any variables that are unassigned
       do not matter. If the expression simplifies to false (zero), then undo the
       last assignment, and assign the opposite value.
-}

module BtBase where
import           Control.Applicative            ( (<|>) )


data Expr = Var Char
          | And Expr Expr
          | Or Expr Expr
          | Not Expr
          | Const Bool
  deriving (Show, Eq)


-- Return the first free variable in a boolean expression.
-- If no free variable, return Nothing
findFree :: Expr -> Maybe Char
findFree (Const _) = Nothing
findFree (Var   c) = Just c
findFree (Not   e) = findFree e
findFree (And a b) = findFree a <|> findFree b
findFree (Or  a b) = findFree a <|> findFree b


-- Guess value of a variable, and replace all occurences of
-- that variable with the guessed value
guessVar :: Char -> Bool -> Expr -> Expr
guessVar var value e =
  let sub = guessVar var value
  in  case e of
        Var c     -> if c == var then Const value else Var c
        And a b   -> And (sub a) (sub b)
        Or  a b   -> Or (sub a) (sub b)
        Not   exp -> Not (sub exp)
        Const _   -> e


-- Simplify an Expr by removing all the Const
simplify :: Expr -> Expr
simplify (Const b) = Const b
simplify (Var   c) = Var c
simplify (Not   e) = case simplify e of
  Const b -> Const (not b)
  simpE   -> Not simpE
simplify (Or a b) =
  let simpEs    = [simplify a, simplify b]
      noFalseEs = filter (/= Const False) simpEs
  in  if Const True `elem` noFalseEs
        then Const True
        else case noFalseEs of
          []       -> Const False
          [e]      -> e
          [e1, e2] -> Or e1 e2
simplify (And a b) =
  let simpEs   = [simplify a, simplify b]
      noTrueEs = filter (/= Const True) simpEs
  in  if Const False `elem` noTrueEs
        then Const False
        else case noTrueEs of
          []       -> Const True
          [e]      -> e
          [e1, e2] -> And e1 e2


-- The driver
sat :: Expr -> Bool
sat e =
  let outputRes res = case res of
        Const b -> b
        _       -> error "Must output result from Const."
  in  case findFree e of
        Nothing -> outputRes e
        Just v  -> sat (simplify (guessVar v True e))
          || sat (simplify (guessVar v False e))
