elementAt :: ([a], Int) -> a
elementAt (x, i) = x !! (i - 1)

main :: IO ()
main = print(elementAt ([1,2,3], 2))
