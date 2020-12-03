data Expr = Val Int | Add Expr Expr
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g expr = case expr of
                  Val x   -> f x
                  Add x y -> g (folde f g x) (folde f g y)

myEval :: Expr -> Int
myEval e = folde (\x -> x) (\x y -> x + y) e

mySize :: Expr -> Int
mySize e = folde (\_ -> 1) (\x y -> x + y) e

t1 :: Expr
t1 = Val 4
-- > myEval t1
-- 4
-- > mySize t1
-- 1


t2 :: Expr
t2 = Add (Val 2) (Val 2)
-- > myEval t2
-- 4
-- > mySize t2
-- 2
