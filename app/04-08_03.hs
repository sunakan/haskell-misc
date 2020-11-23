safetaila :: [a] -> [a]
safetaila list =
    if null list then
        []
    else
        tail list

safetailb :: [a] -> [a]
safetailb list | null list = []
               | otherwise = tail list

safetailc :: [a] -> [a]
safetailc [] = []
safetailc (_:xs) = xs
