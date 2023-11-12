rec interval(x,y) = if y < x then nil else x : interval(x+1,y);;

rec map(f,xs) = if xs=nil then nil else f(head(xs)) : map(f,tail(xs));;

rec sum(xs) = if xs=nil then 0 else head(xs) + sum(tail(xs));;


