val revk(xss) = 
  let val yss = new() in let val ys = new() in
    yss := xss; ys := nil;
    while !yss <> nil do
      (ys := head(!yss) : !ys ; yss := tail(!yss));
    !ys;;
