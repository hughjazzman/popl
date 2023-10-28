val x = new();;
x := 0;;

val a = (lambda () x:= 1; !x): (lambda () x:= 2; !x): (lambda () !x): nil;;

val g = head(a);;
val h = head(tail(a));;
val i = head(tail(tail(a)));;

val f(w, y, z) = y:!x:nil;;

-- x is set by g(), then i() outputs x, then x is set by h()
f(g(), i(), h());;
--> [1, 2]
x := 0;;
-- x is set by h(), then i() outputs x, then x is set by g()
f(h(), i(), g());;
--> [2, 1]
-- f is evaluated last in both cases since if it were first,
-- !x should be 0