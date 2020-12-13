{-
パーサ
-}

anyChar (x:xs) = (x, xs)

test1 xs0 =
  let (x1, xs1) = anyChar xs0
      (x2, xs2) = anyChar xs1
  in ([x1, x2], xs2)

test2 xs0 =
  let (x1, xs1) = test1   xs0
      (x2, xs2) = anyChar xs1
  in (x1 ++ [x2], xs2)

main = do
    print $ anyChar "abc"
    print $ test1 "abc"
    print $ test2 "abc"
