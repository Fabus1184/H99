module Main where

import Test.QuickCheck
import Control.Monad (void, foldM)
import Control.Applicative (liftA2)

import Ten as Ten
import Twenty as Twenty

main :: IO ()
main = do
    let args = Args Nothing 2000 (10 ^ 10) 500 True 10
    void $ mapM_ ($ quickCheckWithResult args)
        [
            Ten.runTests,
            Twenty.runTests
        ]
