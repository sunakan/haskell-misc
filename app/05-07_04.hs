myreplicate :: Int -> a -> [a]
myreplicate len x = [x | _ <- [1..len]]
