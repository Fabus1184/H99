slice :: [a] -> Int -> Int -> [a]
slice _ a b
    | a == b = []
slice x a b = take (b-1) $ drop (a-1) x

rotate :: [a] -> Int -> [a]
rotate x 0 = x
rotate [] _ = []
rotate x n
    | n > 0 = (slice x (n+1) $ length x) ++ (slice x 0 (n+1))
    | n < 0 = reverse $ rotate (reverse x ) (-n)

main :: IO ()
main = do
    print $ rotate ['a','b','c','d','e','f','g','h'] 3
    print $ rotate ['a','b','c','d','e','f','g','h'] (-2)
    return ()
