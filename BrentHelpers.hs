{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module BrentHelpers where
import Data.List ( union, sort, foldl' )
import Data.Containers.ListUtils (nubOrd)


{- ================================================
        MISCELLANEOUS NUMBER STUFF
================================================ -}

isFinite :: RealFloat t => t -> Bool
isFinite x = not (isNaN x || isInfinite x)

-- given an integer, reverses its digits
reverseInt :: Integer -> Integer
reverseInt = read . reverse . show

-- given a float, reverses its digits
reverseFloat :: Double -> Double
reverseFloat = read . reverse . show

-- returns true if a given float is within [epsilon] of an integer value, accurate for x < 1/epsilon
isFloatInteger :: Double -> Bool
isFloatInteger x = abs (x - xRound) < epsilon where
    xRound = fromIntegral $ round x :: Double
    epsilon = 0.0000001

{- ================================================
        LIST/SET MANIPULATION
================================================ -}

{- BUILT-IN SET OPERATIONS in Data.List:
        - nub           : remove duplicate elements
        - nubOrd        : same as nub, but better optimized for sortable stuff
        - intersect a b : find set intersection (will keep duplicates from a)
        - union a b     : find set union (will keep duplicates from a)
        - a \\ b        : find set difference (elements of 'a' will be deleted only _once_ per occurrence in 'b')
-}

-- Custom implementation of splitOn function, working with regular strings.
splitOn :: Char -> String -> [String]
splitOn c str
    | str == ""         = []
    | c `notElem` str   = [str]
    | otherwise         = str_up_to_c : splitOn c str_after_c
    where
        str_up_to_c     = fst broken_string
        str_after_c     = tail $ snd broken_string
        broken_string   = break (== c) str

-- given element x and list l, counts number of occurrences of x in l
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-- given a list of elements, calculates the set of all subsets of that list (order-agnostic)
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs

getAllEq :: (a -> a -> Bool) -> a -> [a] -> ([a],[a])
getAllEq _ _ []     = ([],[])
getAllEq eq t lst   = (filter (eq t) lst, filter (not . eq t) lst)

groupEq :: (a -> a -> Bool) -> [a] -> [[a]]
groupEq eq []           = []
groupEq eq lst@(x:xs)   = groupForX : groupEq eq restOfGroups
    where
        (groupForX, restOfGroups) = getAllEq eq x lst


-- given a list of things that can be negated, alternates their signs in the order +, -, +, -, ...
alternateSigns :: Num a => [a] -> [a]
alternateSigns = zipWith ($) (cycle [id, negate])

-- given a list of things that can be negated, alternates their signs in the order -, +, -, +, ...
alternateSigns_negFirst :: Num a => [a] -> [a]
alternateSigns_negFirst = zipWith ($) (cycle [negate, id])

-- given two integers rMax and cMax, creates an rMax x cMax rectangular array of tuples. Both are numbered from 0 to rMax-1 or to cMax-1. 
-- IMPLEMENTATION: If rMax is nonpositive, result will be an empty list. If rMax is positive but cMax is negative, result will be a list of rMax empty lists.
makeArrayOfIndices :: Int -> Int -> [[(Int, Int)]]
makeArrayOfIndices rMax cMax = [[(i, j) | j <- [0..cMax-1] ] | i <- [0..rMax-1]]

-- Given a 2D list (such as the output of makeArrayOfIndices), applies a function to each element of each row in the 2D list.
map2D :: (a -> b) -> [[a]] -> [[b]]
map2D = map . map

-- Removes the first occurrence of 'elementToRemove' in the list 'listToFilter'
removeFirst :: Eq a => [a] -> a -> [a]
removeFirst listToFilter elementToRemove = case listToFilter of
    []      -> []
    x:xs    ->
        if x==elementToRemove
            then xs
            else x : removeFirst xs elementToRemove

-- Removes all occurrences of 'elementToRemove' in the list 'listToFilter'
removeAll :: Eq a => [a] -> a -> [a]
removeAll listToFilter elementToRemove = case listToFilter of
    []      -> []
    x:xs    ->
        if x==elementToRemove
        then removeAll xs elementToRemove
        else x : removeAll xs elementToRemove

{- ================================================
        TESTING/NARROWING FUNCTIONS
================================================ -}

-- Kronecker delta δ(i,j) := 1 if i==j, 0 otherwise
kronecker :: (Eq a, Num b) => a -> a -> b
kronecker i j
    | i==j          = 1
    | otherwise     = 0

-- rank-2 Levi-Civita symbol
levicivita2 :: (Integral a, Num b) => a -> a -> b
levicivita2 i j
    | i==0 && j==1  = 1
    | i==1 && j==0  = -1
    | otherwise     = 0

-- rank-3 Levi-Civita symbol (computed at runtime)
levicivita3 :: (Integral a, Num b) => a -> a -> a -> b
levicivita3 i j k
    | (i,j,k) `elem` forward_orders         = 1
    | (i,j,k) `elem` backward_orders        = -1
    | otherwise                             = 0
    where
        forward_orders  = swaps_3tuple_2 (0,1,2)
        backward_orders = swaps_3tuple_1 (0,1,2)

-- rank-3 Levi-Civita symbol (pre-computed at write time)
levicivita3' :: (Integral a, Num b) => a -> a -> a -> b
levicivita3' i j k
    | (i,j,k) `elem` forward_orders         = 1
    | (i,j,k) `elem` backward_orders        = -1
    | otherwise                             = 0
    where
        forward_orders  = [(0,1,2), (1,2,0), (2,0,1)]
        backward_orders = [(2,1,0), (1,0,2), (0,2,1)]

-- rank-4 Levi-Civita symbol (computed at runtime)
levicivita4 :: (Integral a, Num b) => a -> a -> a -> a -> b
levicivita4 i j k l
    | (i,j,k,l) `elem` even_parity_orders   = 1
    | (i,j,k,l) `elem` odd_parity_orders    = -1
    | otherwise                             = 0
    where
        even_parity_orders  = swaps_4tuple_2 base
        odd_parity_orders   = swaps_4tuple_1 base `union` swaps_4tuple_3 base
        base = (0,1,2,3)

-- rank-4 Levi-Civita symbol (pre-computed at write time)
levicivita4' :: (Integral a, Num b) => a -> a -> a -> a -> b
levicivita4' i j k l
    | (i,j,k,l) `elem` even_parity_orders   = 1
    | (i,j,k,l) `elem` odd_parity_orders    = -1
    | otherwise                             = 0
    where
        even_parity_orders  = [(0,1,2,3),(2,0,1,3),(3,0,2,1),(1,2,0,3),(1,3,2,0),(1,0,3,2),(3,1,0,2),(2,3,0,1),(2,1,3,0),(3,2,1,0),(0,3,1,2),(0,2,3,1)]
        odd_parity_orders   = [(1,0,2,3),(2,1,0,3),(3,1,2,0),(1,0,2,3),(0,2,1,3),(0,3,2,1),(2,1,0,3),(0,2,1,3),(0,1,3,2),(3,1,2,0),(0,3,2,1),(0,1,3,2),(3,0,1,2),(2,3,1,0),(2,0,3,1),(3,2,0,1),(1,3,0,2),(1,2,3,0)]

-- returns (-1)^n, but way more efficiently than actually multiplying it out
neg1power :: (Integral a, Num b) => a -> b
neg1power n = if even n then 1 else -1

{- ================================================
        COMBINATIONS & PERMUTATIONS
================================================ -}

factorial :: Integral a => a -> a
factorial n = product [1..n]

permutation :: Integral a => a -> a -> a
n `permutation` r = product [n-r+1..n]

combination :: Integral a => a -> a -> a
n `combination` r = (n `permutation` r) `div` factorial r

{- ================================================
        REARRANGEMENTS
================================================ -}
-- for 3-tuples
swaps_3tuple_1 :: Ord a => (a,a,a) -> [(a,a,a)]
swaps_3tuple_1 tuple = nubOrd [singleSwap3ij i j tuple | i <- [0..2], j <- [0..2], i /= j] where
    singleSwap3ij :: (Integral t, Ord a) => t -> t -> (a,a,a) -> (a,a,a)
    singleSwap3ij i j (a0,a1,a2)    --could probably be faster w/ case-of expression
        | (i,j) `elem` [(0,1), (1,0)]   = (a1,a0,a2)
        | (i,j) `elem` [(1,2), (2,1)]   = (a0,a2,a1)
        | (i,j) `elem` [(0,2), (2,0)]   = (a2,a1,a0)
        | otherwise                     = (a0,a1,a2)

swaps_3tuple_2 :: Ord a => (a,a,a) -> [(a,a,a)]
swaps_3tuple_2 = nubOrd . foldl' union [] . map swaps_3tuple_1 . swaps_3tuple_1

-- for 4-tuples
swaps_4tuple_1 :: Ord a => (a,a,a,a) -> [(a,a,a,a)]
swaps_4tuple_1 tuple = nubOrd [singleSwap4ij i j tuple | i <- [0..3], j <- [0..3], i /= j] where
    singleSwap4ij :: (Integral t, Ord a) => t -> t -> (a,a,a,a) -> (a,a,a,a)
    singleSwap4ij i j (a0,a1,a2,a3) --could probably be faster w/ case-of expression
        | (i,j) `elem` [(0,1), (1,0)]   = (a1,a0,a2,a3)
        | (i,j) `elem` [(1,2), (2,1)]   = (a0,a2,a1,a3)
        | (i,j) `elem` [(0,2), (2,0)]   = (a2,a1,a0,a3)
        | (i,j) `elem` [(0,3), (3,0)]   = (a3,a1,a2,a0)
        | (i,j) `elem` [(1,3), (3,1)]   = (a0,a3,a2,a1)
        | (i,j) `elem` [(2,3), (3,2)]   = (a0,a1,a3,a2)
        | otherwise                     = (a0,a1,a2,a3)

swaps_4tuple_2 :: Ord a => (a,a,a,a) -> [(a,a,a,a)]
swaps_4tuple_2 = nubOrd . foldl' union [] . map swaps_4tuple_1 . swaps_4tuple_1

swaps_4tuple_3 :: Ord a => (a,a,a,a) -> [(a,a,a,a)]
swaps_4tuple_3 = nubOrd . foldl' union [] . map swaps_4tuple_2 . swaps_4tuple_1