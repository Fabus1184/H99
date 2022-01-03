dropEvery :: String -> Int -> String

dropEvery _ 1 = ""
dropEvery "" _ = ""

dropEvery x n = dropEvry x n 1

dropEvry :: String -> Int -> Int -> String
dropEvry "" _ _ = []
dropEvry (x:xs) a b
    | b `mod` a == 0 = "" ++ dropEvry xs a (b+1)
    | otherwise = [x] ++ dropEvry xs a (b+1)


main :: IO ()
main = print $ dropEvery "abcdefghik" 3
