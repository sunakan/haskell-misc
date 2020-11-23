-- myand [True, True, True]
-- True
-- myand [True, False, True]
-- False
myand :: [Bool] -> Bool
myand [] = True
myand (x:xs) = if x then myand xs
               else False

-- myconcat [[1,2,3],[4,5],[],[6]]
-- [1,2,3,4,5,6]
myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs

-- myreplicate 3 True
-- [True, True, True]
myreplicate :: Int -> a -> [a]
myreplicate 0 x = []
myreplicate n x = x : myreplicate (n-1) x

-- !!だと被るので!!!
-- [0,1,2,3,4] !!! 3
-- 3
(!!!) :: [a] -> Int -> a
(x:xs) !!! 0 = x
(x:xs) !!! n = xs !!! (n-1)

-- myelem 4 [1,2,3,4,5]
-- True
-- myelem 4 [1,2,3,0,5]
-- False
myelem :: Eq a => a -> [a] -> Bool
myelem e [] = False
myelem e (x:xs) = if e == x then True
                  else myelem e xs
