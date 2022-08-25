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
repli [] _ = []
repli xs 1 = xs
repli (x:xs) n = replicate n x ++ repli xs n

prop_repli :: (Show a, Eq a) => [a] -> NonNegative Int -> Property
prop_repli xs (NonNegative n) =
    (length l === n * length xs)
    .&.
    conjoin ( 
        map (\i -> allEqual (take n (drop i l)))
        [0, n .. length xs - n])
    where
        l = repli xs n
        allEqual xs = all id (zipWith (==) xs (tail xs))

-- Tests

return []
runTests :: (Property -> IO Result) -> IO Bool
runTests = $forAllProperties
