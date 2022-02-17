data Count = Multiple | Single deriving (Show)
data Elem = Elem {
    a :: Count,
    b :: Int,
    c :: Char
}

instance Show Elem where
    show (Elem a b c) = if b == 0 then show a ++ " " ++ show c else show a ++ " " ++ show b ++ " " ++ show c

ss :: (Eq a) => [[a]] -> [[a]]
ss ([]) = []
ss (x:xs)
     | x == head xs = [x ++ head xs] ++ ss ( drop 1 xs )
     | x /= head xs = [x] ++ ss xs

encode :: [Char] -> [Elem]
encode a = map (\s -> if length s /= 1 then Elem {
a = Multiple,
b = length s,
c = head s
} else Elem {
a = Single,
b = 0,
c = s !! 0
}) (ss $ ss ((map (\x -> [x]) a)))

main = print $ encode "aaaabccaadeeee"
