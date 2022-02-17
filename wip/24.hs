import System.Random


draw :: Int -> Int -> Int -> [Int]
draw n m seed = [()] ++ draw (n-1) m seed+1

main :: IO ()
main = do 
    r <- randomIO
    let seed = (r + 1 :: Int)
    print seed
