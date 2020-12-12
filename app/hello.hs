main = do
  putStrLn "Hello"
  name <- getLine
  putStrLn ("Hey " ++ name ++ ", you rock!!")
