-- ある型のキーに別の型の値を結びつける連想リストの型は「キーと値の組」のリスト
type Assoc k v = [(k,v)]
type Subst = Assoc Char Bool

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']


-- True,False
-- A,B,...,Z
-- ￢否定
-- ^論理積
-- A=>B 命題Aならば命題B(A only if B), AはBの十分条件(B if A)
-- A=>B AはBの十分条件(B if A)
-- A=>B BはAの必要条件(A implies B,B is implied by A)
-- False=>FalseはTrue
-- False=>TrueはTrue
-- True=>FalseはFalse
-- True=>TrueはFalse
-- if A then B else True

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop

-- A ^ ￢A
p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

-- (A^B) => A
p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

-- A => (A^B)
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

-- (A^(A=>B)) => B
p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

-- 命題評価関数eval
eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x)   = find x s
eval s (Not p)   = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q
-- 論理包含=>を真理値に対する演算子<=で実装しているところに注意
-- (記号が逆向きだが同じことを表現している)

vars :: Prop -> [Char]
vars (Const _) = []
vars (Var x) = [x]
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

-- > p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
-- > vars p2
-- ['A', 'B', 'A'] or "ABA"

type Bit = Int
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- > int2bin 5
-- [1,0,1]
-- > int2bin 6
-- [0,1,1]
-- > int2bin 1
-- [1]

--make :: Int -> [Bit] -> [Bit]
--make n bits = take n (bits ++ repeat 0)
-- > make 3 [1]
-- [1,0,0]
-- > make 5 [1,0,1]
-- [1,0,1,0,0]


-- bools 3
-- range = [0..(2^3)-1] = [0,1,2,3,4,5,6,7]
--   int2bin
--     = [[0], [1], [0,1], [1,1], [0,0,1], [1,0,1], [0,1,1], [1,1,1]]
--   make 3
--     = [[0,0,0], [1,0,0], [0,1,0], [1,1,0], [0,0,1], [1,0,1], [0,1,1], [1,1,1]]
--   map conv
--     conv 0 = False(F)
--     conv 1 = True(T)
--     = [[F,F,F], [T,F,F], [F,T,F], [T,T,F], [F,F,T], [T,F,T], [F,T,T], [T,T,T]]
--   reverse
--     = [[F,F,F], [F,F,T], [F,T,F], [F,T,T], [T,F,F], [T,F,T], [T,T,F], [T,T,T]]
bools :: Int -> [[Bool]]
bools n = map (reverse . map conv . make n . int2bin) range
          where
            range     = [0..(2^n)-1]
            make n bs = take n (bs ++ repeat 0)
            conv 0    = False
            conv 1    = True

-- bools n って bools (n-1)を含んでる!!
-- bools n って bools (n-1)を複製して、一方の先頭にTrue、他方の先頭にFalse
newBools :: Int -> [[Bool]]
newBools 0 = [[]]
newBools n = map (False:) bss ++ map (True:) bss
             where bss = bools (n-1)

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

-- > rmdups ['A', 'B', 'A']
-- "AB"

substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
           where vs = rmdups (vars p)

-- > vars p
-- "ABA"
-- > rmdups (vars p)
-- "AB"
-- > length (rmdups (vars p2))
-- 2
-- > map (zip (rmdups (vars p2))) (bools (length (rmdups (vars p2))))
-- [[('A',False),('B',False)]
-- ,[('A',False),('B',True)]
-- ,[('A',True),('B',False)]
-- ,[('A',True),('B',True)]
-- ]


isTaut :: Prop -> Bool
isTaut p = and [eval s p | s <- substs p]
-- p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')
-- eval [('A',False),('B',False)] p2
-- and eval [('A',False),('B',False)] p2
-- and eval [('A',False),('B',True)] p2
-- and eval [('A',True),('B',False)] p2
-- and eval [('A',True),('B',True)] p2
