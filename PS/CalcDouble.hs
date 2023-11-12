module CalcDouble where

  data Expr = Val Integer
            | Add Expr Expr
            | Mult Expr Expr
            | Div Expr Expr
            | Fact Expr deriving Show
  eval :: Expr -> Double
  eval (Val n) = fromIntegral n
  eval (Add x y) = (eval x) + (eval y)
  eval (Mult x y) = (eval x) * (eval y)
  eval (Div x y) = (eval x) / (eval y)
  eval (Fact x) = fact (eval x)
  fact x = if x == 0 then 1 else x * (fact (x-1))