insertAt :: a -> [a] -> Int -> [a]
insertAt x [] _ = [x]
insertAt x y n = (take (n-1) y) ++ [x] ++ (drop (n-1) y)

main :: IO ()
main = print $ insertAt 'X' "abcd" 2
