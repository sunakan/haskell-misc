import Data.Char
let2int :: Char -> Int
let2int c = ord c - ord 'a'
int2let :: Int -> Char
int2let n = chr (ord 'a' + n)

largelet2int :: Char -> Int
largelet2int c = ord c - ord 'A'
int2largelet :: Int -> Char
int2largelet n = chr (ord 'A' + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let ((let2int c + n) `mod` 26)
          | isUpper c = int2largelet ((largelet2int c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
