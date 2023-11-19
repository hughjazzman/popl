-- Q1
-- xm = Ok x
(xm $> f) $> g
= ((Ok x) $> f) $> g            -- Definition of M x
= f x $> g                      -- Definition of $>
xm $> (\x -> f x $> g)
= (Ok x) $> (\x -> f x $> g)    -- Definition of M x
= (\x -> f x $> g)(x)           -- Definition of M x
= f x $> g  
-- xm = Fail
(xm $> f) $> g
= (Fail $> f) $> g          -- Definition of M x
= Fail $> g                 -- Definition of $>
= Fail                      -- Definition of $>
xm $> (\x -> f x $> g)
= Fail $> (\x -> f x $> g)  -- Definition of M x
= Fail                      -- Definition of $>                                

(result x) $> f
= (Ok x) $> f   -- Definition of result
= f x           -- Definition of $>

xm $> result
= (Ok x) $> result  -- Definition of M x
= result x          -- Definition of $>
= Ok x              -- Definition of result
= xm                -- Definition of M x

orelse failure ym = ym -- Definition of orelse
orelse ym failure = ym -- ym, if ym = (Ok x) then ym else if ym = failure then ym


-- Q2c
eval (Failure v) env = failure (eval v env)
eval (OrElse e1 e2) env = orelse (eval e1 env) (\v -> eval e2 env $> (\fv -> apply fv [v]) )

-- Q2d
eval (OrElse e1 e2) env = eval e2 env $> (\fv -> orelse (eval e1 env) (\v -> apply fv [v]) )


------ Q3
data Maybe a = Just a | Nothing

type M a = Mem -> Maybe (a, Mem)

-- Q3a
result :: a -> M a
result x mem = Just (x, mem)

($>) :: M a -> (a -> M b) -> M b
($>) xm f mem = let xa = xm mem in 
  case xa of 
    Just (x, mem') -> f x mem'
    Nothing -> Nothing

-- Q3b
new :: M Location
new mem = let (a, mem') = fresh mem in result a mem'

get :: Location -> M Value
get a mem = result (contents mem a) mem

put :: Location -> Value -> M ()
put a v mem = result () (update mem a v)

failure :: M a
failure mem = Nothing

orelse :: M a -> M a -> M a
orelse xm ym mem = let xa = xm mem in 
  case xa of
    Just (x, mem') -> Just (x, mem')
    Nothing -> ym mem

------Q4
-- Q4a
result :: a -> M a
result x mem = (Just x, mem)

($>) :: M a -> (a -> M b) -> M b
($>) xm f mem = let (xa, mem') = xm mem in 
  case xa of 
    Just x -> f x mem'
    Nothing -> (Nothing, mem')

-- Q4b
new :: M Location
new mem = let (a, mem') = fresh mem in result a mem'

get :: Location -> M Value
get a mem = result (contents mem a) mem

put :: Location -> Value -> M ()
put a v mem = result () (update mem a v)

failure :: M a
failure mem = (Nothing, mem)

orelse :: M a -> M a -> M a
orelse xm ym mem = let (xa, mem') = xm mem in 
  case xa of
    Just x -> result x mem'
    Nothing -> ym mem'


-- Q6
etranslate :: Expr -> Expr
etranslate (Assign e1 e2) = Assign e1 (etranslate e2)
etranslate (Apply (Variable f) es) = let v = maybe_find init_env f in
    case v of 
        Just x -> Apply (Variable f) (map etranslate es)
        Nothing -> Apply (Variable f) (map (ref . etranslate) es)
etranslate e = e

dtranslate :: Defn -> Defn
dtranslate (Val x e) = Val x (ref (etranslate e))

ref :: Expr -> Expr
ref e = Apply (Variable "ref") [e]


-- Q7d
*(&x)
= Contents (Address (Variable x))   
= Apply (Variable "!") [Variable x]
= Variable x

-- Q7e
val x = 3;;
*(&x);;
--> 3
&(*x);;
Error: FunC.hs:149:20-58: Non-exhaustive patterns in lambda


-- Q8
fibk :: Int -> (Int -> Ans) -> Ans
fibk n k = if n <= 1 then k n else fibk (n-1) (\r1 -> fibk (n-2) (\r2 -> k (r1 + r2)))

-- Q9
(xm $> f) $> g
= (\k -> xm (\x -> f x k)) $> g                         -- Definition of $>
= (\k' -> (\k -> xm (\x -> f x k)) (\x' -> g x' k'))    -- Definition of $>
= (\k' -> xm (\x -> f x (\x' -> g x' k')))              -- Simplify lambda k
xm $> (\x -> f x $> g)
= \k -> xm (\x' -> (\x -> f x $> g) x' k)           -- Definition of $>
= \k -> xm (\x' -> (f x' $> g) k)                   -- Simplify lambda x
= \k -> xm (\x' -> (\k' -> f x' (\x -> g x k')) k)  -- Definition of $>
= \k -> xm (\x' -> (f x' (\x -> g x k)))            -- Simplify lambda k'
= (xm $> f) $> g                                    -- Set k = k', x' = x, x = x'                          

(result x) $> f
= \k -> (result x) (\x' -> f x' k)      -- Definition of $>
= \k -> (\k' -> k' x) (\x' -> f x' k)   -- Definition of result
= \k -> ((\x' -> f x' k) x)             -- Simplify lambda k'
= \k -> f x k                           -- Simplify lambda x'
= f x                                   -- Definition of f x

xm $> result
= (\k -> k x) $> result                     -- Definition of M x
= \k' -> (\k -> k x) (\x -> result x k')    -- Definition of $>
= \k' -> ((\x -> result x k') x)            -- Simplify lambda k
= \k' -> result x k'                        -- Simplify lambda x
= \k' -> k' x                               -- Definition of result
= xm                                        -- Definition of M x
