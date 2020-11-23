-- 1. 型を定義
-- 2. 場合分けをする
-- 3. 場合分けの簡単な方を定義
-- 4. 場合分けの複雑な方を定義
-- 5. 一般化して単純にする

-- asum [1,2,3,4]
-- 10
-- 1/5. asum :: Num a => [a] -> a
-- 2/5. asum [] =
-- 2/5. asum (x:xs) =
-- 3/5. asum [] = 0
-- 4/5. asum (x:xs) = x + asum xs
-- 5/5. 一般化?????
asum :: Num a => [a] -> a
asum [] = 0
asum (x:xs) = x + asum xs

-- btake 3 [0,1,2,3,4,5,6]
-- [0,1,2]
-- 1/5. btake :: Int -> [a] -> [a]
-- 2/5. btake 0 _ =
-- 2/5. btake n [] =
-- 2/5. btake n (x:xs) =
-- 3/5. btake 0 _ = []
-- 3/5. btake n [] = []
-- 4/5. btake n (x:xs) = x : btake (n-1) xs
-- 5/5. 一般化?????
btake :: Int -> [a] -> [a]
btake 0 _ = []
btake n [] = []
btake n (x:xs) = x : btake (n-1) xs

-- clast [0,1,2,3,4,5,6]
-- 6
-- 1/5. clast :: [a] -> a
-- 2/5. clast (x:[]) =
-- 2/5. clast (x:xs) =
-- 3/5. clast (x:[]) = x
-- 4/5. clast (x:xs) = clast xs
-- 5/5. 一般化?????
clast :: [a] -> a
clast (x:[]) = x
clast (x:xs) = clast xs
