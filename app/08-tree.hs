data Tree a = Leaf a | Node (Tree a) a (Tree a)
t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 8))

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r

-- > occurs 3 t
-- True
-- > occurs 2 t
-- False

flatten :: Tree a -> [a]
flatten (Leaf x) = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

-- > flatten t
-- [1,3,4,5,6,7,8]

occurs2 :: Ord a => a -> Tree a -> Bool
occurs2 x (Leaf y) = x == y
occurs2 x (Node l y r) | x == y    = True
                       | x < y     = occurs2 x l
                       | otherwise = occurs2 x r
-- > occurs2 2 t
-- False
-- > occurs2 3 t
-- True

-- Data with Leaf only
data Tree2 a = Leaf2 a | Node2 (Tree2 a) (Tree2 a)
-- Data with Node only
data Tree3 a = Leaf3 | Node3 (Tree3 a) a (Tree3 a)
-- Data-A with Leaf, Data-B with Node
data Tree4 a b = Leaf4 a | Node4 (Tree4 a b) b (Tree4 a b)
-- Node have SubTree, Leaf is empty list
data Tree5 a = Node5 a [Tree5 a]
