-- euclid 6 27
-- 3
euclid x y | x > y     = euclid (x-y) y
           | x < y     = euclid x (y-x)
           | otherwise = x
