-- a
-- all :: (a -> Bool) -> [a] -> Bool
myAll :: (a -> Bool) -> [a] -> Bool
myAll p []     = True
myAll p (x:xs) = if p x
                 then myAll p xs
                 else False
myAll2 :: (a -> Bool) -> [a] -> Bool
myAll2 p = and . map p

-- b
-- any :: (a -> Bool) -> [a] -> Bool
myAny :: (a -> Bool) -> [a] -> Bool
myAny p []     = False
myAny p (x:xs) = if p x
                 then True
                 else myAny p xs
myAny2 :: (a -> Bool) -> [a] -> Bool
myAny2 p = or . map p

-- c
-- takeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p xs = let sub ans p list = case list of
                                        []     -> ans
                                        (x:xs) -> if p x
                                                  then sub (x : ans) p xs
                                                  else ans
                   in sub [] p xs
myTakeWhile2 :: (a -> Bool) -> [a] -> [a]
myTakeWhile2 _ []                 = []
myTakeWhile2 p (x:xs) | p x       = x : myTakeWhile2 p xs
                      | otherwise = []

-- d
-- dropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile p []     = []
myDropWhile p (x:xs) = if p x
                       then myDropWhile p xs
                       else x : xs

myDropWhile2 :: (a -> Bool) -> [a] -> [a]
myDropWhile2 _ []                 = []
myDropWhile2 p (x:xs) | p x       = myDropWhile2 p xs
                      | otherwise = x : xs
