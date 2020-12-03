type Assoc k v = [(k,v)]
type Subst = Assoc Char Bool

-- もともと
-- data Prop = Const Bool
--           | Var Char
--           | Not Prop
--           | And Prop Prop
--           | Imply Prop Prop

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          | Or Prop Prop
          | Eq Prop Prop

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

-- 命題評価関数eval
eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Or p q)    = eval s p || eval s q
eval s (Eq p q)    = eval s p == eval s q

vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q)    = vars p ++ vars q
vars (Eq p q)    = vars p ++ vars q

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
             where bss = bools (n-1)

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)

isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]

-- (A^(A=>B)) => B
p1 :: Prop
p1 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')
-- > isTaut p1
-- True

-- AvB
p2 :: Prop
p2 = Or (Var 'A') (Var 'B')
-- > isTaut p2
-- False

-- (AvB) => A
p3 :: Prop
p3 = Imply (Or (Var 'A') (Var 'B')) (Var 'A')
