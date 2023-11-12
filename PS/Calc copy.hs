module Calc where

  data Expr = Val Integer
            | Add Expr Expr
            | Mult Expr Expr
            | Div Expr Expr
            | Fact Expr deriving Show

  eval :: Expr -> Integer
  eval (Val n) = n
  eval (Add x y) = (eval x) + (eval y)
  eval (Mult x y) = (eval x) * (eval y)
  eval (Div x y) = div (eval x) (eval y)
  eval (Fact x) = fact (eval x)

  fact = \n -> if n==0 then 1 else n * (fact (n-1))
