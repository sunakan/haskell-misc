-- mymerge [2,5,6] [1,3,4]
-- [1,2,3,4,5,6]
mymerge :: Ord a => [a] -> [a] -> [a]
mymerge [] [] = []
mymerge xs [] = xs
mymerge [] ys = ys
mymerge (x:xs) (y:ys) | x < y  = x : mymerge xs (y:ys)
                      | x >= y = y : mymerge (x:xs) ys

-- myhalve [3,2,1,3,4,5,6,1,7]
-- ([3,2,1,3],[4,5,6,1,7])
myhalve :: [a] -> ([a], [a])
myhalve xs = (take n xs, drop n xs)
             where n = (length xs) `div` 2


-- mymsort [3,2,1,3,4,5,6,1,7]
-- [1,1,2,3,3,4,5,6,7]
mymsort :: Ord a => [a] -> [a]
mymsort [] = []
mymsort (x:[]) = [x]
mymsort list = mymerge (mymsort xs) (mymsort ys)
               where (xs, ys) = myhalve list
