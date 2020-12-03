data Expr = Val Int | Add Expr Expr
folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g expr = case expr of
                  Val x   -> f x
                  Add x y -> g (folde f g x) (folde f g y)
