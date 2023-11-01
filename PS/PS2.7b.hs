-- Q7(b)

eval :: Expr -> Env -> (Value -> Answer) -> Answer

eval (Apply f es) env k = 
  evalargs es env (\ args -> 
    eval f env (\ fv ->
      apply fv args k))
--   eval f env (\ fv ->
--     evalargs es env (\ args ->
--       apply fv args k))

evalargs :: [Expr] -> Env -> ([Value] -> Answer) -> Answer
evalargs [] env k = k []
evalargs (e:es) env k =
  evalargs es env (\ vs -> eval e env 
      (\v -> k (v:vs)))
-- evalargs (e:es) env k =
--   eval e env (\ v -> evalargs es env
--                         (\ vs -> k (v:vs)))