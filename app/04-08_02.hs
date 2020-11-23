thirda :: [a] -> a
thirda list = head (tail (tail list))

thirdb :: [a] -> a
thirdb list = list !! 2

thirdc :: [a] -> a
thirdc (_:_:x:_) = x
