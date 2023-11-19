val x = new();;
put(x, 1);;
(x:=2; failure) orelse (get(x));;

-- Q7e
val f(x) = x;;
f(1);;
--> 1;;
(&(*f))(1);;
Error: FunC.hs:149:20-58: Non-exhaustive patterns in lambda
