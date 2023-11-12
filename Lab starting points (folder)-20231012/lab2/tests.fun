val x = new();;
x := 3;;

val f(y) = x := 4; y;;

val g(z) = 1;;
val h(z) = z;;

-- f is not evaluated
g(f(!x));;
!x;;
--> 3

-- f is evaluated
h(f(!x));;
!x;;
--> 4

-- Jensen's device for Fun with name parameters

val sum(i, a, b, f) =
  let val s = new() in
  i := a; s := 0;
  -- call-by-name
  -- while !i < b do (s := !s + f; i := !i + 1);
  -- call-by-value
  while !i < b do (s := !s + f(); i := !i + 1);
  !s;;

val go() =
  let val i = new() in
  -- call-by-name
  -- sum(i, 0, 10, !i * !i );;
  -- call-by-value
  sum(i, 0, 10, lambda () !i * !i );;

-- Call-by-value solution also works in call-by-name