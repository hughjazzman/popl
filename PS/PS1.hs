-- Q2(a)
index :: (Eq a) => a -> [a] -> Int
-- index x [] = -1
-- index x (y:ys)
--     | x == y            = 0
--     | index x ys == -1  = -1
--     | otherwise         = 1 + index x ys
index x (y:ys) = index' x (y:ys) 0 where
    index' :: (Eq a) => a -> [a] -> Int -> Int
    index' x [] n = -1
    index' x (y:ys) n
        | x == y    = n
        | otherwise = index' x ys (n + 1)
        
-- Q2(c)
indexc :: (Eq a) => a -> [a] -> Int
indexc x y = 
    let z = [i | (i, j) <- zip [0..(length y)] y, j == x] in 
        if null z then -1 
        else head z