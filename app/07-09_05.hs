-- curry
myCurry :: ((a,b) -> c) -> (a -> b -> c)
myCurry f = (\x y -> f (x,y))

-- uncurry
myUncurry :: (a -> b -> c) -> ((a,b) -> c)
myUncurry f = (\(x, y) -> f x y)
