-- Q7c
val swap(a,b) = let val t = *a in  *a := *b; *b:= t; t;;

val x = 3;;
val y = 4;;
x:y:nil;;
--> [3, 4]
swap(&x, &y);;
--> 3
x:y:nil;;
--> [4, 3]
