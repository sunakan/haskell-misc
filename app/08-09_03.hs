data Tree a = Leaf a | Node (Tree a) (Tree a)
-- 全てのNodeに対して、右と左の部分木にある葉の数が高々1つだけ異なる時、
-- 木は平衡していると呼ぶ。
-- Leafは平衡している
-- 2分木が平衡しているかどうかを調べる関数
-- balanced :: Tree a -> Bool
leaves :: Tree a -> Int
leaves (Leaf _)   = 1
leaves (Node a b) = leaves a + leaves b

balanced :: Tree a -> Bool
balanced (Leaf _) = True
-- NG : balanced (Node a b) = abs ((leaves a) - (leaves b)) <= 1
balanced (Node a b) = abs ((leaves a) - (leaves b)) <= 1
                      && balanced a && balanced b

t1 :: Tree Int
t1 = Leaf 0

t2 :: Tree Int
t2 = Node (Leaf 1) (Leaf 1)

t3 :: Tree Int
t3 = Node (Node (Leaf 1) (Leaf 1)) (Leaf 1)

t4 :: Tree Int
t4 = Node (Node (Node (Leaf 1) (Leaf 1)) (Leaf 1)) (Leaf 1)

t5 :: Tree Int
t5 = Node (Node (Node (Node (Leaf 1) (Leaf 1)) (Leaf 1)) (Leaf 1)) (Node (Node (Node (Leaf 1) (Leaf 1)) (Leaf 1)) (Leaf 1))

-- *Main> balanced t1
-- True
-- *Main> balanced t2
-- True
-- *Main> balanced t3
-- True
-- *Main> balanced t4
-- False
-- *Main> balanced t5
-- False
