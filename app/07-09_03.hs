-- foldrを使ってmap f
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x xs -> f x : xs) []

-- foldrを使って filter p
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p xs = foldr (\x y -> if p x then x:y else y) [] xs
myFilter2 :: (a -> Bool) -> [a] -> [a]
myFilter2 p = foldr (\x xs -> if p x then x : xs else xs) []
