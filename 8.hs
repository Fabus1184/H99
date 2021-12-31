compress :: (Eq a) => [a] -> [a]
compress (x:xs)
    | xs == [] = [x]
    | x == head xs = compress(xs)
    | otherwise = [x] ++ compress(xs)

main :: IO ()
main = print $ compress "aaaabccaadeeee"
