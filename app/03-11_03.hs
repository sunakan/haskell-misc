second xs = head (tail xs)
-- second :: [a] -> a

swap (x,y) = (y,x)
-- swap :: (a,b) = (b,a)

pair x y = (x,y)
-- pair :: a -> b -> (a,b)

double x = x * 2
-- double :: Num a => a -> a

palindrome xs = reverse xs == xs
-- double :: Eq a => [a] -> Bool

twice f x = f (f x)
-- twice :: (a -> a) -> a -> a
