-- mymerge [2,5,6] [1,3,4]
-- [1,2,3,4,5,6]
mymerge :: Ord a => [a] -> [a] -> [a]
mymerge [] [] = []
mymerge xs [] = xs
mymerge [] ys = ys
mymerge (x:xs) (y:ys) | x < y  = x : mymerge xs (y:ys)
                      | x >= y = y : mymerge (x:xs) ys
