rec interval(x,y) = if y < x then nil else x : interval(x+1,y);;

rec map(f,xs) = if xs=nil then nil else f(head(xs)) : map(f,tail(xs));;

rec sum(xs) = if xs=nil then 0 else head(xs) + sum(tail(xs));;

rec (-)(x,y) = x + m1 * y;;

rec pow(x,y) = if y=0 then 1 else x * pow(x,y-1);;

rec sumpow(x,n) = sum(map(lambda (y) pow(x,y),interval(1,n)));;

