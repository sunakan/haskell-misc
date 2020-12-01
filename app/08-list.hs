data List a = Nil | Cons a (List a)
len :: List a -> Int
len Nil         = 0
len (Cons _ xs) = 1 + len xs

-- > len (Cons 3 Nil)
-- 1
-- > len (Cons 3 (Cons 3 Nil))
-- 2
-- > len (Cons 3 (Cons 3 (Cons 3 Nil)))
-- 3
