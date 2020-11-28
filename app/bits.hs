import Data.Char
type Bit = Int

-- 0:   "0000 0000" : [0,0,0,0, 0,0,0,0,0]
-- 1:   "1000 0000" : [1,0,0,0, 0,0,0,0,0]
-- 2:   "0100 0000" : [0,1,0,0, 0,0,0,0,0]
-- 3:   "1100 0000" : [1,1,0,0, 0,0,0,0,0]
-- 4:   "0010 0000" : [0,0,1,0, 0,0,0,0,0]
-- 5:   "1010 0000" : [1,0,1,0, 0,0,0,0,0]
-- ...
-- 255: "1111 1111" : [1,1,1,1, 1,1,1,1,1]
bin2int :: [Bit] -> Int
--bin2int bits = sum [w*b | (w,b) <- zip weights bits]
--               where weights = iterate (*2) 1
-- foldrを使うと..
bin2int = foldr (\x y -> x + 2*y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

-- ((Int -> Char) . ([Bit] -> Int)) . ([Bit] -> [[Bit]])
decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8


transmit :: String -> String
transmit = decode . channel . encode
channel :: [Bit] -> [Bit]
channel = id

-- >>> :load bits.hs
-- >>> transmit "Hello World"
-- "Hello World"
