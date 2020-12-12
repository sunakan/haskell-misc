-- :load するときはコメントアウトする
main :: IO ()
main = print (solutions [1,3,7,10,25,50] 765)

-- A: 1~10
-- B: 1~10
-- C: 25
-- D: 50
-- E: 75
-- F: 100
-- Goal = 100 ~ 999

data Op = Add | Sub | Mul | Div
instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"

-- Valid: 答えが正の整数になるものだけ
valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Sub x y = x > y
valid Mul _ _ = True
valid Div x y = (x `mod` y) == 0

-- Apply: 適用
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr
instance Show Expr where
  show (Val n)     = show n
  show (App o l r) = brak l ++ show o ++ brak r
                     where
                      brak (Val n) = show n
                      brak e       = "(" ++ show e ++ ")"

-- > show (App Add (Val 1) (App Mul (Val 2) (Val 3)))
-- "1+(2*3)"
-- > show (App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10)))
-- "(1+50)*(25-10)"

-- Values: 式の中の数値をリストとして返す
values :: Expr -> [Int]
values (Val n)     = [n]
values (App _ l r) = values l ++ values r
-- > values (Val 2)
-- [2]
-- > values (App Add (Val 2) (Val 3))
-- [2,3]


-- Eval: 式全体
eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]
-- > eval (App Add (Val 2) (Val 3))
-- [5]
-- > eval (App Sub (Val 2) (Val 3))
-- []



-- Subs: リストの部分リスト
subs :: [a] -> [[a]]
subs []     = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs
-- > subs [1,2,3]
-- [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]



-- Interleave: 新しい要素を入れて返す
interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)
-- > interleave 5 [1,2,3]
-- [[5,1,2,3],[1,5,2,3],[1,2,5,3],[1,2,3,5]]



-- Perms: リストの要素に対する順列を返す
perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))
-- > perms [1,2,3]
-- [[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]



-- Choices: リストから選択肢を返す
choices :: [a] -> [[a]]
choices = concat . map perms . subs
-- > choices [1,2,3]
-- [[],[3],[2],[2,3],[3,2],[1],[1,3],[3,1],[1,2],[2,1],[1,2,3],[2,1,3],[2,3,1],[1,3,2],[3,1,2],[3,2,1]]



-- Solution: 答え
-- 補足1
-- elem :: a -> [a] -> Bool
-- elemは第1引数がlistに含んでいるかどうか
-- values 3 [1,2,3,4]
-- True
-- values [1,2] [[1,2], [3,4], [5,6]]
-- True
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
  elem (values e) (choices ns) && eval e == [n]
-- solution e [1,3,7,10,25,50] 765
-- の時、e=(1+50)*(25-10)を表現していた時
-- > solution (App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))) [1,3,7,10,25,50] 765
-- True
-- 理由：values eで[1,50,25,10]
-- choices [1,3,7,10,25,50]で全て順番の組み合わせを出す
-- [1,50,25,10]がある



-- あるリストを2つの空ではないリストに分ける
split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs) : [(x:ls,rs) | (ls,rs) <- split xs]
-- > split [1,2,3,4]
-- [([1],[2,3,4]), ([1,2],[3,4]), ([1,2,3],[4])]


-- 演算子一覧
ops :: [Op]
ops = [Add,Sub,Mul,Div]

-- Combine: 演算子の組み合わせ
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]
-- > combine (Val 2) (Val 3)
-- [2+3,2-3,2*3,2/3]


exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls,rs) <- split ns,
                  l      <- exprs ls,
                  r      <- exprs rs,
                  e      <- combine l r]
-- > exprs [1,2,3]
-- [1+(2+3),1-(2+3),1*(2+3),1/(2+3),1+(2-3),1-(2-3),1*(2-3),1/(2-3),1+(2*3),1-(2*3),1*(2*3),1/(2*3),1+(2/3),1-(2/3),1*(2/3),1/(2/3),(1+2)+3,(1+2)-3,(1+2)*3,(1+2)/3,(1-2)+3,(1-2)-3,(1-2)*3,(1-2)/3,(1*2)+3,(1*2)-3,(1*2)*3,(1*2)/3,(1/2)+3,(1/2)-3,(1/2)*3,(1/2)/3]



-- A: 1~10
-- B: 1~10
-- C: 25
-- D: 50
-- E: 75
-- F: 100
-- Goal = 100 ~ 999
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]
-- choices nsで全ての順と組み合わせを出す
-- exprs ns'で全ての演算子の組み合わせも出す
-- eval eで式を計算&&Valid

-- > solutions [1,3,7,10,25,50] 765
-- たくさん
