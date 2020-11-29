org :: (a -> a) -> [a] -> (a -> Bool) -> [a]
org f xs p = [f x | x <- xs, p x]

ans :: (a -> a) -> [a] -> (a -> Bool) -> [a]
ans f xs p = map f (filter p xs)
