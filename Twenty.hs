{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Twenty where

import Test.QuickCheck
import Control.Monad (liftM)
import GHC.Generics (Generic)
import Data.List (nub)
import Ten (pack, encode)

-- Problem 11

data N a = Single a | Multiple Int a
    deriving (Show, Eq)

encodeModified :: (Eq a) => [a] -> [N a]
encodeModified [] = []
encodeModified [x] = [Single x]
encodeModified xs = map em' $ encode xs
    where
        em' (1, x) = Single x
        em' (n, x) = Multiple n x

prop_encodeModified :: (Eq a, Show a) => [a] -> Property
prop_encodeModified xs =
        conjoin (map (\i -> ex (l !! i) =/= ex (l !! (i + 1))) [0 .. length l - 2])
        .&&.
        (sum (map n l) === length xs)
    where
        l = encodeModified xs
        ex (Single x) = x
        ex (Multiple _ x) = x
        n (Single _) = 1
        n (Multiple n' _) = n'

-- Problem 12

decodeModified :: [N a] -> [a]
decodeModified [] = []
decodeModified (Single x : xs) = x : decodeModified xs
decodeModified (Multiple n x : xs) = replicate n x ++ decodeModified xs

prop_decodeModified :: (Eq a, Show a) => [a] -> Property
prop_decodeModified xs = decodeModified (encodeModified xs) === xs

-- Problem 13

encodeDirect :: (Eq a) => [a] -> [N a]
encodeDirect = encodeModified

prop_encodeDirect :: (Show a, Eq a) => [a] -> Property
prop_encodeDirect = prop_encodeModified

-- Problem 14

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x, x] ++ dupli xs

prop_dupli :: (Show a, Eq a) => [a] -> Property
prop_dupli xs =
    (length l === 2 * length xs)
    .&.
    conjoin (map (\i ->
        if even i then
            l !! i === l !! (i + 1)
        else
            l !! i === l !! (i - 1)
    ) [0 .. length l - 2])
    where
        l = dupli xs

-- Problem 15

repli :: [a] -> Int -> [a]
repli xs n = concatMap (replicate n) xs

prop_repli :: (Show a, Eq a) => [a] -> NonNegative Int -> Property
prop_repli xs (NonNegative n) =
    (length l === n * length xs)
    where
        l = repli xs n
        allEqual xs = all id (zipWith (==) xs (tail xs))

-- Problem 16

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = map fst $ filter (\(_, i) -> i `mod` n /= 0) $ zip xs [1 ..]

prop_dropEvery :: (Show a, Eq a) => [a] -> Positive Int -> Property
prop_dropEvery xs (Positive n) = 
    length (dropEvery xs n) === (length xs - (length xs `div` n))

-- Problem 17

split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split (x:xs) 0 = ([], x:xs)
split (x:xs) n = (\(a, b) -> (a ++ [x], b)) $ split xs (n - 1)

prop_split :: (Show a, Eq a) => [a] -> NonNegative Int -> Property
prop_split xs (NonNegative n)
    | n > length xs = True === True
    | otherwise =
        (s === length xs)
        .&.
        (length (fst l) === n)
        .&.
        (length (snd l) === length xs - n)
   where
        l = split xs n
        s = (\(a, b) -> length a + length b) l

-- Problem 18

slice :: [a] -> Int -> Int -> [a]
slice xs a b = take (b - a) (drop a xs)

prop_slice :: (Show a, Eq a) => [a] -> NonNegative Int -> NonNegative Int -> Property
prop_slice xs (NonNegative a) (NonNegative b)
    | a > length xs = null l === True
    | otherwise =
        (b < a ==> null l)
    where
        l = slice xs a b

-- Problem 19

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate (x:xs) n = rotate (xs ++ [x]) (n - 1)

prop_rotate :: (Show a, Eq a) => [a] -> NonNegative Int -> Property
prop_rotate xs (NonNegative n) = length (rotate xs n) === length xs

-- Problem 20

removeAt :: [a] -> Int -> [a]
removeAt [] _ = []
removeAt (x:xs) 0 = xs
removeAt (x:xs) n = x : removeAt xs (n - 1)

prop_removeAt :: (Show a, Eq a) => [a] -> NonNegative Int -> Property
prop_removeAt [] (NonNegative n) = null (removeAt [] n) === True
prop_removeAt xs (NonNegative n)
    | n >= length xs = removeAt xs n === xs
    | otherwise = length (removeAt xs n) === length xs - 1

-- Tests

return []
runTests :: (Property -> IO Result) -> IO Bool
runTests = $forAllProperties
