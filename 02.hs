myButLast :: [a] -> a
myButLast x = x !! (length(x) - 2)

main :: IO ()
main = print $ myButLast [1..10]
