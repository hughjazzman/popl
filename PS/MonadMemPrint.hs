module MonadMemPrint where
import Data.List

type Value = Int

infixl 1 $>

type M a = [Value] -> (String, a, [Value])
type Location = Int

result :: a -> M a
result x = \mem -> ("",x,mem)

($>) :: M a -> (a -> M b) -> M b
($>) xm f =
  \mem -> let (s,x,mem') = xm mem in let (t,y,mem'') = f x mem' in (s ++ t, y, mem'')

put :: Location -> Value -> M ()
put a v = \mem -> ("", (), (take a mem) ++ v : (drop (a+1) mem))

get :: Location -> M Value
get a = \mem -> ("", mem !! a , mem)

new :: M(Location)
new = \mem -> ("",length mem,mem++[0])

output :: String -> M()
output s = \mem -> (s, (), mem)

test = new $> \a ->
    put a 40 $> \_ ->
    get a $> \v ->
    put a (v+1) $> \_ ->
    output "hello " $> \_ -> 
    output "world" $> \_ ->
    get a $> \v -> 
    result (v+1)
    -- nil
-- test []
-- ("hello world",42,[41]) 

