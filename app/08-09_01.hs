-- multi
-- use add
data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add m n = int2nat (nat2int m + nat2int n)

newAdd :: Nat -> Nat -> Nat
newAdd Zero n     = n
newAdd (Succ m) n = Succ (newAdd m n)

-- newAdd (Succ (Succ Zero)) (Succ Zero)
-- Succ (newAdd (Succ Zero) (Succ Zero))
-- Succ (Succ (newAdd Zero (Succ Zero)))
-- Succ (Succ (Succ Zero))

mult :: Nat -> Nat -> Nat
mult Zero n        = Zero
mult _    Zero     = Zero
mult m    (Succ n) = newAdd m (mult m n)
