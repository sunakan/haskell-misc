-- scalarproduct [1,2,3] [4,5,6]
-- 32
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct a b = sum [x*y| (x,y) <- zip a b]
