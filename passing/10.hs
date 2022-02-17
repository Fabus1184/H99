ss :: (Eq a) => [[a]] -> [[a]]

ss ([]) = []
ss (x:xs)
     | x == head xs = [x ++ head xs] ++ ss ( drop 1 xs )
     | x /= head xs = [x] ++ ss xs

encode a = (map (\s -> (length s, head s)) (ss $ ss ((map (\x -> [x]) a))))

main = print $ encode "aaaabccaadeeee"
