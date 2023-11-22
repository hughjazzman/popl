module Fungol(main) where
import Parsing
import Environment
import Memory
import FunSyntax
import FunParser


-- MONAD

infixl 1 $>

data Maybe a = Just a | Nothing

type M a = Mem -> (Fungol.Maybe a, Mem)

result x mem = (Fungol.Just x, mem)

(xm $> f) mem = let (xa, mem') = xm mem in
  case xa of
    Fungol.Just x -> (f $! x) mem'
    Fungol.Nothing -> (Fungol.Nothing, mem')

get :: Location -> M Value
get a mem = result (contents mem a) mem

put :: Location -> Value -> M ()
put a v mem = result () (update mem a v)

new :: M Location
new mem = let (a, mem') = fresh mem in result a mem'

exit :: M a
exit mem = (Fungol.Nothing, mem)

orelse :: M a -> M a -> M a
orelse xm ym mem = let (xa, mem') = xm mem in 
  case xa of
    Fungol.Just x -> result x mem'
    Fungol.Nothing -> ym mem'

bind :: Value -> M Location
bind v = new $> (\ a -> put a v $> (\ () -> result a))


-- SEMANTIC DOMAINS

data Value =
    IntVal Integer 
  | BoolVal Bool
  | Nil | Cons Value Value

data Def =
    Const Value
  | Ref Location
  | Proc ([Value] -> M Value)

type Env = Environment Def
type Mem = Memory Value


-- EVALUATOR

eval :: Expr -> Env -> M Value

eval (Number n) env = result (IntVal n)

eval (Variable x) env = 
  case find env x of
    Const v -> result v
    Ref a -> get a
    _ -> error "variable does not have a value"

eval (Apply (Variable x) es) env =
  mapm (\ e -> eval e env) es $> (\ args ->
    apply (find env x) args)

eval (If e1 e2 e3) env =
  eval e1 env $> (\b ->
    case b of
      BoolVal True -> eval e2 env
      BoolVal False -> eval e3 env
      _ -> error "boolean required in conditional")

eval (Lambda xs e1) env =
  error "no lambda expressions"

eval (Let d e1) env =
  elab d env $> (\env' -> eval e1 env')

eval (Assign (Variable x) e2) env =
  case find env x of
    Ref a ->
      eval e2 env $> (\ v2 -> put a v2 $> (\ () -> result v2))
    _ -> error "assigning to a non-variable"

eval (Sequence e1 e2) env =
  eval e1 env $> (\v -> eval e2 env)

eval (While e1 e2) env = u
  where
    u = eval e1 env $> (\v1 ->
      case v1 of
        BoolVal True -> eval e2 env $> (\v2 -> u)
        BoolVal False -> result Nil
        _ -> error "boolean required in while loop")

eval (Loop e) env = u
  where
    u =  orelse (eval e env $> \_ -> u) (result Nil)

eval Exit env = exit

eval e env =
  error ("can't evaluate " ++ pretty e)

mapm :: (a -> M b) -> [a] -> M [b]
mapm f [] = result []
mapm f (x:xs) =
  f x $> (\y -> mapm f xs $> (\ys -> result (y:ys)))

abstract :: [Ident] -> Expr -> Env -> Def
abstract xs e env =
  Proc (\ args -> 
    mapm bind args $> (\ as ->
      eval e (defargs env xs (map Ref as))))

apply :: Def -> [Value] -> M Value
apply (Proc f) args = f args
apply _ argms = error "applying a non-procedure"

elab :: Defn -> Env -> M Env

elab (Val x e) env = 
  eval e env $> (\ v -> 
    bind v $> (\ a -> result (define env x (Ref a))))

elab (Rec x (Lambda xs e1)) env =
  let env' = define env x (abstract xs e1 env') in result env'

elab (Rec x _) env =
  error "RHS of letrec must be a lambda"


-- INITIAL ENVIRONMENT

init_env :: Env
init_env =
  make_env [("nil", Const Nil), 
    ("true", Const (BoolVal True)), ("false", Const (BoolVal False)),
    pureprim "+" (\ [IntVal a, IntVal b] -> IntVal (a + b)),
    pureprim "-" (\ [IntVal a, IntVal b] -> IntVal (a - b)),
    pureprim "*" (\ [IntVal a, IntVal b] -> IntVal (a * b)),
    pureprim "div" (\ [IntVal a, IntVal b] ->
      if b == 0 then error "Dividing by zero" else IntVal (a `div` b)),
    pureprim "mod" (\ [IntVal a, IntVal b] ->
      if b == 0 then error "Dividing by zero" else IntVal (a `mod` b)),
    pureprim "~" (\ [IntVal a] -> IntVal (- a)),
    pureprim "<" (\ [IntVal a, IntVal b] -> BoolVal (a < b)),
    pureprim "<=" (\ [IntVal a, IntVal b] -> BoolVal (a <= b)),
    pureprim ">" (\ [IntVal a, IntVal b] -> BoolVal (a > b)),
    pureprim ">=" (\ [IntVal a, IntVal b] -> BoolVal (a >= b)),
    pureprim "=" (\ [a, b] -> BoolVal (a == b)),
    pureprim "<>" (\ [a, b] -> BoolVal (a /= b)),
    pureprim "integer" (\ [a] ->
      case a of IntVal _ -> BoolVal True; _ -> BoolVal False),
    pureprim "head" (\ [Cons h t] -> h),
    pureprim "tail" (\ [Cons h t] -> t),
    pureprim ":" (\ [a, b] -> Cons a b)]
  where
    constant x v = (x, Const v)
    pureprim x f = (x, Proc (\ args -> result $ primwrap x f args))
    primitive x f = (x, Proc (primwrap x f))


-- AUXILIARY FUNCTIONS ON VALUES

instance Eq Value where
  IntVal a == IntVal b = a == b
  BoolVal a == BoolVal b = a == b
  Nil == Nil = True
  Cons h1 t1 == Cons h2 t2 = h1 == h2 && t1 == t2
  _ == _ = False

instance Show Value where
  show (IntVal n) = show n
  show (BoolVal b) = if b then "true" else "false"
  show Nil = "[]"
  show (Cons h t) = "[" ++ show h ++ shtail t ++ "]"
    where 
      shtail Nil = ""
      shtail (Cons h t) = ", " ++ show h ++ shtail t
      shtail x = " . " ++ show x

instance Show Def where
  show (Const v) = show v
  show (Ref a) = "<ref " ++ show a ++ ">"
  show (Proc _) = "<proc>"


-- MAIN PROGRAM

type GloState = (Env, Mem)

obey :: Phrase -> GloState -> (String, GloState)
obey (Calculate exp) (env, mem) =
  let (xa, mem') = eval exp env mem in
  case xa of 
    Fungol.Just v -> (print_value v, (env, mem'))
    Fungol.Nothing -> ("exit!", (env, mem'))
obey (Define def) (env, mem) =
  let x = def_lhs def in
  let (Fungol.Just env', mem') = elab def env mem in
  (print_defn env' x, (env', mem'))

main = dialog funParser obey (init_env, init_mem)
