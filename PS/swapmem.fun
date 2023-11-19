-- Q7a
val swap(a,b) = let val t = !a in a := !b; b := t;;

val x = new();;
x := 3;;
val y = new();;
y := 4;;
swap(x,y);;
!x;;
--> 4
!y;;
--> 3