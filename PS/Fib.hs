infixl 1 $>

type M a = (a -> Ans) -> Ans

result :: a -> M a
result x k = k x

($>) :: M a -> (a -> M b) -> M b
($>) xm f k = xm (\x -> f x k)

fib :: Integer -> Integer
fib x = if x<=1 then x else fib(x-1) + fib(x-2)

fibk :: Integer -> (Integer -> Ans) -> Ans
fibk x k = if x<=1 then k x else fibk(x-1)(\r -> fibk(x-2)(\s -> k(r+s)))

fibm :: Integer -> M Integer
fibm x = if x <= 1 then result x else fibm (x-1) $> \r -> fibm (x-2) $> \s -> result (r+s)

fact :: Integer -> Integer
fact x = if x==0 then 1 else x * fact(x-1)

fack :: Integer -> (Integer -> Ans) -> Ans
fack x k = if x==0 then k 1 else fack (x-1) (\r -> k (x * r))
