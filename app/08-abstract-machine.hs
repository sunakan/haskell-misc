-- Evaluate=評価する
-- Expression=(数)式
data Expr = Val Int | Add Expr Expr

value :: Expr -> Int
value (Val n)   = n
value (Add x y) = value x + value y
-- (2 + 3) + 4
-- value (Add (Add (Val 2) (Val 3)) (Val 4))
-- value (Add (Val 2) (Val 3)) + value (Val 4)
-- (value (Val 2) + value (Val 3)) + value (Val 4)
-- (2 + value (Val 3)) + value (Val 4)
-- (2 + 3) + value (Val 4)
-- 5 + value (Val 4)
-- 5 + 4
-- 9

-- Opは "式を評価する" || "Intを加算"
type Cont = [Op]
data Op = EVAL Expr | ADD Int

-- 式が整数であれば、評価済みだから、制御スタックの命令を実行
eval :: Expr -> Cont -> Int
eval (Val n)   c = exec c n
eval (Add x y) c = eval x (EVAL y : c)

exec :: Cont -> Int -> Int
exec []           n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD n : c)  m = exec c (n + m)

newValue :: Expr -> Int
newValue e = eval e []
-- > newValue (Add (Add (Val 2) (Val 3)) (Val 4))
-- 9

-- 簡約
-- newValue (Add (Add (Val 2) (Val 3)) (Val 4))
-- eval (Add (Add (Val 2) (Val 3)) (Val 4)) []
-- eval (Add (Val 2) (Val 3)) [EVAL (Val 4)]
-- eval (Val 2) [EVAL (Val 3), EVAL (Val 4)]
-- exec [EVAL (Val 3), EVAL (Val 4)] 2
-- eval (Val 3) [ADD 2, EVAL (Val 4)]
-- exec [ADD 2, EVAL (Val 4)] 3
-- exec [EVAL (Val 4)] (2 + 3)
-- exec [EVAL (Val 4)] 5
-- eval (Val 4) [Add 5]
-- exec [Add 5] 4
-- exec [] (5 + 4)
-- exec [] 9
-- 9
