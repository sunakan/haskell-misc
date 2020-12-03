data Tree a = Leaf a | Node (Tree a) (Tree a)

leaves :: Tree a -> Int
leaves (Leaf _)   = 1
leaves (Node a b) = leaves a + leaves b

-- 空ではない整数のリストを平衡木に変換する関数
-- balance :: [a] -> Tree a
balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs  = Node (balance (take half xs)) (balance (drop half xs))
               where half = (length xs) `div` 2
