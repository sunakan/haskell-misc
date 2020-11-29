-- Use foldl
-- >>> dec2int [2,3,4,5]
-- 2345
dec2int :: [Int] -> Int
dec2int xs = foldl (\x sum -> x*10 + sum) 0 xs

dec2int2 :: Num a => [a] -> a
dec2int2 = foldl (\x y -> x*10 + y) 0

-- dec2int2 [1,2,3,4]
-- (0*10 + 1)
-- ((0*10 + 1)*10 + 2)
-- (((0*10 + 1)*10 + 2)*10 + 3)
-- ((((0*10 + 1)*10 + 2)*10 + 3)*10 + 4)

dec2int3 :: Num a => [a] -> a
dec2int3 = foldr (\x y -> y) 0
