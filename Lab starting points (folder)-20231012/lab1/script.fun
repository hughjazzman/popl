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
  let rec loop(yss, ys, s, done) =
    -- flag variable 
    if done then s
    else if yss = nil then 
      -- if yss empty, check ys
      if integer(ys) then loop(nil, nil, s + ys, true)
      else loop(append(tail(ys), yss), head(ys), s, done) 
    -- break down yss
    else if integer(ys) then loop(tail(yss), head(yss), s + ys, done)
    -- break down ys if still not an integer
    else if ys <> nil then loop(append(tail(ys), yss), head(ys), s, done) 
    -- set flag if only left with s
    else loop(nil, nil, s, true) in
  loop(xss, 0, 0, false);;

val flatsum2(xss) = 
  let val yss = new() in let val ys = new() in let val s = new() in let val d = new() in
  yss := xss; ys := 0; s := 0; d := false;
  while !d = false do
    (if !yss = nil then
      (if integer(!ys) then (s := !s + !ys;  d := true)
      else (yss := tail(!ys); ys := head(!ys)))
    -- order of assignment matters here
    else if integer(!ys) then (s := !s + !ys; ys := head(!yss); yss := tail(!yss))
    else if !ys <> nil then (yss := append(tail(!ys), !yss); ys := head(!ys))
    else d := true);
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