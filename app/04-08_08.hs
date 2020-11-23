luhnDouble :: Int -> Int
luhnDouble x =
  if (x*2) > 9 then
    (x*2) - 9
  else
    (x*2)

luhn :: Int -> Int -> Int -> Int -> Int
luhn a b c d =
  s `mod` 10
  where s = sum (luhnDouble a : b : luhnDouble c : d : [])
