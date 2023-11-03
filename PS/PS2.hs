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

-- Q7(b)
eval :: Expr -> Env -> (Value -> Answer) -> Answer

eval (Apply f es) env k = 
  evalargs es env (\ args -> 
    eval f env (\ fv ->
      apply fv args k))

evalargs :: [Expr] -> Env -> ([Value] -> Answer) -> Answer
evalargs [] env k = k []
evalargs (e:es) env k =
  evalargs es env (\ vs -> eval e env 
      (\v -> k (v:vs)))

-- Q9
mapm :: (a -> M b) -> [a] -> M [b]
mapm f [] = result []
mapm f (e:es) = 
  f e $> \v -> 
  mapm f es $> \vs -> 
  result (v:vs)

-- Q10
mmap :: (a -> b) -> (M a -> M b)
mmap f = \xm -> xm $> \a -> result (f a)

join :: M (M a) -> M a
join mma = mma $> \ma -> ma

mmap id 
= \xm -> xm $> \a -> result (id a)
= \xm -> xm $> \a -> result a 
= \xm -> xm $> result
= \xm -> xm
= id

mmap g . mmap f
= \xm -> mmap f xm $> \a -> result (g a)
= \xm -> xm $> \a' -> result (f a') $> \a -> result (g a)
= \xm -> xm $> \a' -> result (g (f a'))
= mmap (g . f)

mmap f . result
= \x -> result x $> \a -> result (f a)
= \x -> result (f x)
= result . f

join . mmap (mmap f)
= \xmm -> mmap (mmap f) xmm $> \xm -> xm
= \xmm -> xmm $> \xm' -> result ((mmap f) xm') $> \xm -> xm
= \xmm -> xmm $> \xm' -> result (xm' $> \x -> result (f x)) $> \xm -> xm
= \xmm -> xmm $> \xm' -> (xm' $> \x -> result (f x))
= (\xmm -> xmm $> \xm' -> xm') $> \x -> result (f x)
= (\xmm -> join xmm) $> \x -> result (f x)
= mmap f . join

join . mmap join
= \xmm -> mmap join xmm $> \xm -> xm
= \xmm -> xmm $> \xm -> result (join xm) $> \xm -> xm
= \xmm -> xmm $> \xm -> join xm
= \xmm -> xmm $> \xm -> xm $> \x -> x
= \xmm -> join xmm $> \x -> x
= join . join

join . result
= \xmm -> result xmm $> \xmm' -> xmm'
= \xmm -> xmm
= id

join . mmap result
= \xmm -> mmap result xmm $> \xmm' -> xmm'
= \xmm -> xmm $> \a -> result (result a) $> \xmm' -> xmm'
= \xmm -> xmm $> \a -> result a
= \xmm -> xmm
= id