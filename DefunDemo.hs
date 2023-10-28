module DefunDemo where

  data FUNCTION = DOUBLE | SQUARE | MULT Integer

  apply :: FUNCTION -> Integer -> Integer
  apply DOUBLE x = x * 2
  apply SQUARE x = x * x
  apply (MULT n) x = x * n

  mymap f [] = []
  mymap f (x : xs) = (apply f x) : (mymap f xs)
  -- mymap f [] = []
  -- mymap f (x : xs) = (f x) : (mymap f xs)


  evens = mymap DOUBLE [1..10]
  -- evens = mymap (\x -> x*2) [1..10]
  squares = mymap SQUARE [1..10]
  -- squares = mymap (\x -> x*x) [1..10]
  table n = mymap (MULT n) [1..10]
  -- table n = mymap (\x -> x*n) [1..10]
  
