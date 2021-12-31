import Debug.Trace

ss (x:xs)
     | x == head xs = [x ++ head xs] ++ ss ( drop 1 xs )
     | x /= head xs = [x] ++ ss xs
     | otherwise = []

main = do
    let x = ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
    let y = (map (\x -> [x]) x)
    print $ ss $ ss y
