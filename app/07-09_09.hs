-- >>> altMap (+10) (+100) [0,1,2,3,4]
-- [10,101,12,103,14]
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _  _  []                           = []
altMap f1 f2 (x:xs) | odd (length (x:xs)) = f1 x : altMap f1 f2 xs
                    | otherwise           = f2 x : altMap f1 f2 xs

altMap2 :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap2 _  _  []     = []
altMap2 f1 f2 (x:xs) = f1 x : altMap2 f2 f1 xs
