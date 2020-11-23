halve :: [a] -> ([a], [a])
halve list = let halflen = (length list) `div` 2 in (take halflen list, drop halflen list)
