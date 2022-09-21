{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Thirty where

import Test.QuickCheck
import System.Random
import Data.List (permutations)

-- Problem 21

insertAt :: [a] -> a -> Int -> [a]
insertAt [] e _ = [e]
insertAt (x:xs) e 0 = e : x : xs
insertAt (x:xs) e n = x : insertAt xs e (n - 1)

prop_insertAt :: (Show a, Eq a) => [a] -> a -> NonNegative Int -> Property
prop_insertAt xs x (NonNegative n)
    | n >= length xs = last (insertAt xs x n) === x
    | otherwise = length (insertAt xs x n) === length xs + 1

-- Problem 22

range :: Int -> Int -> [Int]
range a b = [a .. b]

prop_range :: Int -> Int -> Property
prop_range a b
    | b > a = length (range a b) === b - a + 1
    | b == a = range a b === [a]
    | b < a = range a b === []

-- Problem 23

rnd_select :: Int -> Int -> IO [Int]
rnd_select 0 _ = pure []
rnd_select n x = mapM (const $ randomRIO (1, x)) [1 .. n]

-- Problem 24

rnd_permu :: [Int] -> IO [Int]
rnd_permu [] = pure []
rnd_permu [x] = pure [x]
rnd_permu xs = do
    i <- randomRIO (0, product [1 .. length xs])
    pure $ permutations xs !! i

-- Tests

return []
runTests :: (Property -> IO Result) -> IO Bool
runTests = $forAllProperties
