repli :: String -> Int -> String
repli "" _ = ""
repli (x:xs) n = times x n ++ repli xs n

times :: Char -> Int -> [Char]
times _ 0 = ""
times x n = [x] ++ times x (n-1)

main :: IO ()
main = print $ repli "abc" 3
