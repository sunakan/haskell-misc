luhnDouble :: Int -> Int
luhnDouble x =
  if (x*2) > 9 then
    (x*2) - 9
  else
    (x*2)

-- 4桁版
luhn :: Int -> Int -> Int -> Int -> Int
luhn a b c d =
  s `mod` 10
  where s = sum (luhnDouble a : b : luhnDouble c : d : [])

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _  _  []     = []
altMap f1 f2 (x:xs) = f1 x : altMap f2 f1 xs

-- >>> newLuhn [1,7,8,4]
-- True
-- >>> newLuhn [4,7,8,3]
-- False
newLuhn :: [Int] -> Bool
newLuhn xs =
  (s `mod` 10) == 0
    where s = sum (altMap (luhnDouble) (id) xs)
