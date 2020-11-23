mylength :: [a] -> Int
mylength [] = 0
mylength (x:xs) = 1 + mylength xs
-- mylength [1,2,3]
-- 1 + (1 + (1 + 0))

mydrop :: Int -> [a] -> [a]
mydrop _ [] = []
mydrop 0 xs = xs
mydrop n (x:xs) = mydrop (n-1) xs
-- mydrop 3 [1,2,3,4,5]
-- mydrop 2 [2,3,4,5]
-- mydrop 1 [3,4,5]
-- mydrop 0 [4,5]
-- [4,5]

myinit :: [a] -> [a]
myinit [x] = []
myinit (x:xs) = x : (myinit xs)
