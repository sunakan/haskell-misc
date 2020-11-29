import Data.Char
type Bit = Int

--- add parity bit
addParityBit :: [Bit] -> [Bit]
addParityBit bits = length (filter odd bits) `mod` 2 : bits

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- n個ずつに分ける
myChop :: Int -> [Bit] -> [[Bit]]
myChop _ []   = []
myChop n bits = take n bits : myChop n (drop 9 bits)

checkParity :: [Bit] -> [Bit]
checkParity (x:xs) | x == checkedParity = xs
                   | otherwise          = error "Parity error"
                     where checkedParity = length (filter odd xs) `mod` 2

-- [1,0,1,1,0,0,0,0] => 13
bin2int :: [Bit] -> Int
bin2int xs = foldr (\x y -> x + y*2) 0 xs

-- [1,0,1,1,0,0,0,0] => 13
int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

-- "hello" => [[0,0,0,1,0,1,1,0],[1,0,1,0,0,1,1,0],[0,0,1,1,0,1,1,0],[0,0,1,1,0,1,1,0],[1,1,1,1,0,1,1,0]]
--         => [[1,0,0,0,1,0,1,1,0],[0,1,0,1,0,0,1,1,0],[0,0,0,1,1,0,1,1,0],[0,0,0,1,1,0,1,1,0],[0,1,1,1,1,0,1,1,0]]
--         => [1,0,0,0,1,0,1,1,0,0,1,0,1,0,0,1,1,0,0,0,0,1,1,0,1,1,0,0,0,0,1,1,0,1,1,0,0,1,1,1,1,0,1,1,0]
encode :: String -> [Int]
encode = concat . map (addParityBit . make8 . int2bin . ord)

decode :: [Bit] -> String
decode bits = map chr (map bin2int (map checkParity (myChop 9 bits)))
decode2 :: [Bit] -> String
decode2 = map (chr.bin2int.checkParity).myChop 9

channel :: [Bit] -> [Bit]
channel = id

brokenChannel :: [Bit] -> [Bit]
brokenChannel = tail

-- "hello world" => "hello world"
transmit :: String -> String
transmit = decode2 . channel . encode

transmit2 :: String -> String
transmit2 = decode2 . brokenChannel . encode
