module ExprBase where

data Expr = Var Char
          | And Expr Expr
          | Or Expr Expr
          | Not Expr
          | Const Bool
    deriving (Show, Eq)
