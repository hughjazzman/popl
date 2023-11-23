val x = 0;;

val ex1 = loop (
    x := x + 2;
    if x > 3 then exit else nil;
    x := x + 3
);;

val ex2 = let rec f() = exit in loop f();;
val ex3 = loop(let rec f() = exit in f());;
val ex4 = loop(let rec f() = exit in loop f());;

ex1;;
-- exit in def for FungolCont.hs
-- ex2;;
ex3;;
-- fails to evaluate for Fungol.hs
ex4;;

-- In ex2, the exit is fine for Fungol.hs since