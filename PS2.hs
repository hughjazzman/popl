-- Q3
eval :: Expr -> Env -> Mem -> (Value, Mem)
eval (Repeat e1 e2) env mem = f mem
  where
    f mem =
      let (a, mem') = eval e1 env mem in
      let (b, mem'') = eval e2 env mem' in
      case b of
        BoolVal False -> f mem''
        BoolVal True -> (Nil, mem'')
        _ -> error "boolean required in expression 2"

-- Q4
primitive "put" (\ [Addr a, b] mem -> (Nil, update mem a b))

