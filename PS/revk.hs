type Answer = [Int]

revk :: [Int] -> ([Int] -> Answer) -> Answer
revk [] k = k []
revk (x:xs) k = revk xs (\ys -> k (ys ++ [x]))

revit :: [Int] -> [Int] -> [Int]
revit [] ys = ys
revit (x:xs) ys = revit xs ([x] ++ ys)