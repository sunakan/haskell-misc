-- Evaluate=評価する
-- Expression=(数)式
data Expr = Val Int
          | Add Expr Expr
          | Mult Expr Expr

-- Opは "式を評価する" || "Intを加算"
type Cont = [Op]
data Op = EVALa Expr
        | EVALm Expr
        | ADD Int
        | MULT Int

-- 式が整数であれば、評価済みだから、制御スタックの命令を実行
eval :: Expr -> Cont -> Int
eval (Val n)    c = exec c n
eval (Add x y)  c = eval x (EVALa y : c)
eval (Mult x y) c = eval x (EVALm y : c)

exec :: Cont -> Int -> Int
exec []            n = n
exec (EVALa y : c) n = eval y (ADD n : c)
exec (EVALm y : c) n = eval y (MULT n : c)
exec (ADD n : c)   m = exec c (n + m)
exec (MULT n : c)  m = exec c (n * m)

newValue :: Expr -> Int
newValue e = eval e []
