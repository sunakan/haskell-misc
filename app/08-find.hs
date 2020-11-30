type Pair a = (a,a)
type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']
-- > :type find
-- find :: Eq k => k -> Assoc k v -> v

-- > find 4 [(1,"a"),(2,"b"),(3,"c"),(4,"d"),(5,"e"),(4,"f")]
-- "d"
