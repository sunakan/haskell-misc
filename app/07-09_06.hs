-- unfold
myUnfold p h t x | p x = []
                 | otherwise = h x : myUnfold p h t (t x)

int2bin :: Int -> [Int]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

newInt2bin :: Int -> [Int]
newInt2bin = myUnfold (== 0) (`mod` 2) (`div` 2)

type Bit = Int
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

newChop8 :: [Bit] -> [[Bit]]
newChop8 = myUnfold (== []) (take 8) (drop 8)

-- map f
myMap :: Eq a => (a -> b) -> [a] -> [b]
myMap f  = myUnfold (== []) (f . head) (tail)
