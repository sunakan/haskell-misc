-- 型IO aを持つ式 == アクションと呼ぶ
-- IO Charは文字を返すアクション
-- IO ()は結果に意味がないことを示すユニットを返すアクション
----IO ()だと副作用目的がわかる

-- type IO a = World -> (a,World)

-- Char -> IO Int は
-- カリー化された Char -> World -> (Int,World) の別名

-- 基本アクション
-- getChar
-- putChar
-- return

{-------------------------------------------------
do v1 <- a1
   v2 <- a2
   ...
   vn <- an
   return (f v1 v2 ... vn)
-- アクションa1の結果はv1
-- アクションa2の結果はv2
-- ...
-}------------------------------------------------



-- 行の読み込み
-- 組み込み
--getLine :: IO String
--getLine = do x <- getChar
--             if x == '\n' then
--                return []
--             else
--                do xs <- getLine
--                   return (x:xs)


-- 出力
-- 組み込み
--putStr :: String -> IO ()
--putStr []     = return ()
--putStr (x:xs) = do putChar x
--                   putStr xs

-- 組み込み
--putStrLn :: String -> IO ()
--putStrLn xs = do putStr xs
--                 putChar '\n'


strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters"

-- > strlen
-- Enter a string: <<何か入力>>
-- The string has X characters
