val counter =
let val x = new() in
 x:=0 ;
 (lambda () x:=!x+1) : (lambda () !x) : nil;;

val incr = head(counter);;
val print = head(tail(counter));;
