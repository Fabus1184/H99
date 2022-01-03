split :: String -> Int -> (String, String)

split "" _ = ("", "")
split x 0 = ("", x)

split x n = (take n x, drop n x)

main :: IO ()
main = print $ split "abcdefghik" 3
