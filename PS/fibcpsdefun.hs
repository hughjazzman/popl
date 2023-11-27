type Answer = Int
data Cont = Show | Add Int Cont | FibAdd Int Cont

appcont :: Cont -> Int -> Answer
appcont Show r = r
appcont (Add n k) r = appcont k (r + n)
appcont (FibAdd n k) r = fibk (n-2) (Add r k)

fibk :: Int -> Cont -> Answer
fibk n k = if n <= 1 then appcont k n else fibk (n-1) (FibAdd n k)