{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Ten where

import Test.QuickCheck
import Control.Monad (liftM)
import GHC.Generics (Generic)
import Data.List (nub)

-- Problem 1

myLast :: [a] -> a
myLast [] = error "myLast: empty List"
myLast [x] = x
myLast (_:xs) = myLast xs

prop_myLast :: (Show a, Eq a) => NonEmptyList a -> Property
prop_myLast (NonEmpty xs) = myLast xs === last xs

-- Problem 2

myButLast :: [a] -> a
myButLast (x:[y]) = x
myButLast (_:xs) = myButLast xs
myButLast _ = error "myButLast: empty List"

prop_myButLast :: (Show a, Eq a) => NonEmptyList a -> Property
prop_myButLast (NonEmpty xs)
    | length xs < 2 = discard
    | otherwise = myButLast xs === (xs !! (length xs - 2))

-- Problem 3

elementAt :: [a] -> Int -> a
elementAt [] _ = error "elementAt: index too large"
elementAt (x:xs) n
    | n == 0 = x
    | otherwise = elementAt xs (n - 1)

prop_elementAt :: (Show a, Eq a) => NonEmptyList a -> NonNegative Int -> Property
prop_elementAt (NonEmpty xs) (NonNegative n)
    | n >= length xs = discard
    | otherwise = elementAt xs n === xs !! n

-- Problem 4

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

prop_myLength :: (Show a) => [a] -> Property
prop_myLength xs = myLength xs === length xs

-- Problem 5

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

prop_myReverse :: (Eq a, Show a) => [a] -> Property
prop_myReverse xs = myReverse xs === reverse xs

-- Problem 6

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

prop_isPalindrome :: (Show a, Eq a) => [a] -> Property
prop_isPalindrome xs
    | isPalindrome xs = isPalindrome xs ==> xs === reverse xs
    | otherwise = isPalindrome (xs ++ reverse xs) === True

-- Problem 7

data NestedList a = Elem a | List [NestedList a]
    deriving (Show, Eq, Generic)

instance (Arbitrary a) => Arbitrary (NestedList a) where
    arbitrary = sized nl'
        where
            nl' 0 = liftM Elem arbitrary
            nl' n = oneof [liftM Elem arbitrary, liftM List (scale (`div` 3) (listOf arbitrary))]
    shrink x = shrink' x ++ genericShrink x
        where
            shrink' (List []) = []
            shrink' (List _) = [List []]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List xs) = concatMap flatten xs

prop_flatten :: (Show a, Eq a) => NestedList a -> Property
prop_flatten l = flatten l === flatten l

-- Problem 8

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs)
    | x == y = compress (x:xs)
    | otherwise = x : compress (y:xs)

prop_compress :: (Eq a, Show a) => [a] -> Property
prop_compress xs = all id (map (\i -> l !! i /= l !! (i + 1)) [0 .. length l - 2]) === True
    where
        l = compress xs

-- Problem 9

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = (x : eqs) : pack (drop (length eqs) xs)
    where
        eqs = takeWhile (== x) xs

prop_pack :: (Eq a, Show a) => [a] -> Property
prop_pack xs 
    | all ((== 1) . length) (pack xs) = discard
    | otherwise = conjoin $ map (\i -> head (l !! i) =/= head (l !! (i + 1))) [0 .. length l - 2]
    where l = pack xs

-- Problem 10

encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode [x] = [(1, x)]
encode xs = map (\l -> (length l, head l)) (pack xs)

prop_encode :: (Eq a, Show a) => [a] -> Property
prop_encode xs
    | all ((== 1) . fst) (encode xs) = discard
    | otherwise =
        conjoin (map (\i -> snd (l !! i) =/= snd (l !! (i + 1))) [0 .. length l - 2])
        .&&.
        (sum (map fst l) === length xs)
    where
        l = encode xs 

-- Tests

return []
runTests :: (Property -> IO Result) -> IO Bool
runTests = $forAllProperties
