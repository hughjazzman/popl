fib :: Integer -> Integer
fib x = if x <= 1 then x else fib(x-1) + fib(x-2)

fibk :: Integer -> (Integer -> Ans) -> Ans
fibk x k = if x <= 1 then k x else
           fibk (x-1) $ \r -> fibk (x-2) $ \s -> k (r+s)

fibm :: Integer -> M Integer
fibm x = if x <= 1 then result x else
         fibm (x-1) $> \r -> fibm (x-2) $> \s -> result (r+s)

infix 1 $>
type Ans = Integer
type M a = (a -> Ans) -> Ans

result :: a -> M a
result x = \k -> k x

($>) :: M a -> (a -> M b) -> M b
($>) xm f = \k -> xm (\x -> f x k)

-- lem is an example of a program that uses its continuation
-- multiple times.
lem :: M (Either a (a -> Ans))
lem k = k (Right (\x -> k (Left x)))
-- An intuition is that the first time it returns Right a label, a jump point.
-- When jumped to, lem starts again, but returning Left this time. 


-- Jump is a program that discards its continuation.
jmp :: (a -> Ans) -> a -> M Ans
jmp l x k = l x
-- If its first argument is regarded as a label, this jumps back to the label
-- with the given argument. 

factlem :: Integer -> M Integer
factlem n = if n==1 then result 1 else
            lem $> \x ->
            case x of 
              Right l -> factlem (n-1) $> \r -> jmp l r
              Left r -> result (r * n)

-- Setjmp returns a pair of a value and a program counter
-- The first time the value is the argument to setjmp.
-- When you jump back, the value is the argument to jmp. 
setjmp :: a -> M (a , a -> Ans)
setjmp x = lem $> \z ->
           case z of Right l -> result (x,l)
                     Left y -> setjmp y
-- Equivalently, expanding the definition of lem:
-- setjmp x k = k (x , \y -> setjmp y k)

-- "Very imperative" version of factorial
-- where we maintain state (x,r),
-- argument x and accumulator r,
-- and jump to do the recursion. 
fact :: Integer -> M Integer
fact n = setjmp (n,1) $> \((x,r),l) ->
         if x == 1 then result r
         else jmp l (x-1,r*x)
