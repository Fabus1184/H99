data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> a
flatten (List []) = List []

main :: IO ()
main = do
    print $ flatten (Elem 5)
    print $ flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
    print $ flatten (List [])
    return ()   
