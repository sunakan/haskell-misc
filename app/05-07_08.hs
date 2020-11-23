find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

-- positions False [True, False, True, False, True]
-- [1,3]
positions :: Eq a => a -> [a] -> [Int]
positions x list = find x (zip list [0..])
