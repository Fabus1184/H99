slice :: [a] -> Int -> Int -> [a]
slice _ a b
    | a == b = []
slice x a b = take (b-1) $ drop (a-1) x

main :: IO ()
main = print $ slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
