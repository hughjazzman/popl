-- Wira Azmoon Ahmad
-- PoPL Lab 1

rec append(xs, ys) = 
  if xs = nil then ys else head(xs) : append(tail(xs), ys);;

rec concat(xss) = 
  if xss = nil then nil else append(head(xss), concat(tail(xss)));;

rec flatten(xss) = 
  if xss = nil then nil 
  else if integer(xss) then list(xss)
  else append(flatten(head(xss)), flatten(tail(xss)));;

rec flatsum(xss) = 
  if xss = nil then 0
  else if integer(xss) then xss
  else flatsum(head(xss)) + flatsum(tail(xss));;

rec flatsum1(xss) = 
  let rec loop(yss, s) =
    let val ys = head(yss) in 
    if yss = nil then s
    else 
      -- if head is empty list ignore it
      if ys = nil then loop(tail(yss), s)
      -- if head is integer, add it then proceed
      else if integer(ys) then loop(tail(yss), s + ys)
      -- otherwise, add the head back to be processed
      else loop(append(ys, tail(yss)), s) in
  loop(xss, 0);;

val flatsum2(xss) = 
  let val yss = new() in let val s = new() in
    yss := xss; s := 0;
    while !yss <> nil do
      if head(!yss) = nil then yss := tail(!yss)
      else if integer(head(!yss)) then (s := !s + head(!yss); yss := tail(!yss))
      else yss := append(head(!yss), tail(!yss));
  !s;;
  
val xxx = list(list(1,2), nil, list(3,4));;
val yyy = list(1, list(2, 3), list(4, list(5), 6));;

append(list(1, 2, 3), list(4, 5, 6));;
--> [1, 2, 3, 4, 5, 6]

concat(xxx);;
--> [1, 2, 3, 4]
-- concat(yyy);;
--> Error: bad arguments to primitive: tail(1)
flatten(yyy);;
--> [1, 2, 3, 4, 5, 6]
flatsum(yyy);;
--> 21
flatsum1(yyy);;
--> 21
flatsum2(yyy);;
--> 21