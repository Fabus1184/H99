import System.Random
import Data.List

rnd_select :: [a] -> Int -> [a]
rnd_select x n = map (x!!) is
    where is = take n . nub $ randomRs (0, length x - 1) (mkStdGen 100)

main :: IO ()
main = do
    print $ rnd_select "abcdefgh" 3 
    return ()
