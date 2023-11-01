-- Q1
rec foldl(f, x, xs) = 
  if xs = nil then x else foldl(f, f(x, head(xs)), tail(xs));;

rec foldr(f, x, xs) = 
  if xs = nil then x else f(head(xs), foldr(f, x, tail(xs)));;

rec append(xs, ys) = 
  if xs = nil then ys else head(xs) : append(tail(xs), ys);;

val map(f, xs) = 
  let val g(a, b) = append(a, f(b):nil) in
  foldl(g, nil, xs);;

-- Q2(b)
rec index(x, xs, n) =
  if xs = nil then -1 
  else if x = head(xs) then n
  else index(x, tail(xs), n+1);;

-- Q3
-- Number 3
-- Variable "x"
-- Apply (Variable "+") (Variable "x", Number 3)
-- Apply (Variable "*") [Apply (Variable "+") (Variable "x", Number 3), Number 4]
-- (Let (Val "x" (Number 2)) (Apply (Variable "+") [Variable "x",Number 3]))
-- Let (Val "f" (Lambda ["x"] (Apply (Variable "+") [Variable "x",Number 3]))) (Apply (Variable "f") [Number 2])
-- Apply (Variable "f") [Apply (Variable "g") [Variable "x"]]
-- Apply [Apply (Variable "f") (Variable "g")] [Variable "x"]

-- Q4

-- Q5
-- Yes legal
-- --> 24

-- Q6
-- f is a function that adds 3 to its argument when set,
-- it does not depend on x once created

