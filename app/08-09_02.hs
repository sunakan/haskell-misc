-- Prelude
-- data Ordering = LT | EQ |GT
-- compare :: Ord a => a -> a -> Ordering

-- 探索木
data Tree a = Leaf a | Node (Tree a) a (Tree a)
t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 8))
oldOccurs :: Eq a => a -> Tree a -> Bool
oldOccurs x (Leaf y) = x == y
oldOccurs x (Node l y r) = x == y || oldOccurs x l || oldOccurs x r

newOccurs :: Ord a => a -> Tree a -> Bool
newOccurs x (Leaf y) = x == y
newOccurs x (Node l y r) = case (compare x y) of
                             LT -> newOccurs x l
                             EQ -> True
                             GT -> newOccurs x r
-- 全探索しなくていいから効率的である
-- *Main> newOccurs 2 t
-- False
-- *Main> newOccurs 1 t
-- True
-- *Main> newOccurs 5 t
-- True
