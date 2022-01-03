data NestedList a = Elem a | List [NestedList a]


flatten :: NestedList a -> List [a]
flatten (x) = []
-- kaputt

main :: IO ()
main = do
    print $ flatten (List [])
    print $ flatten (Elem 5)
    print $ flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
    return ()   
