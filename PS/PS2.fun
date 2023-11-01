-- Q1
val x1 = new();;
val y1 = x1;;

val x2 = new();;
val y2 = new();;

val f(x,y) = 
    x := 1; y := 2;
    !x = !y;;

f(x1, y1);;
--> true
f(x2, y2);;
--> false

-- Q2
val a = new();;
val b = new();;

b := a;;
!b = a;;
--> true
a := 2;;
!!b;;
--> 2

val c = new();;
val clst = 1:c;;
c := clst;;
clst;;
--> [1 . <address 3>]
!tail(clst);;
--> [1 . <address 3>]
