-- ^でも^^でも被るので^^^で表現
(^^^) :: Int -> Int -> Int
m ^^^ 0         = 1
m ^^^ n | n > 0 = m * (m ^^^ (n-1))

-- 2^3 = 2 * (2 * (2 * 1))
