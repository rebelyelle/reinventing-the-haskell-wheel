{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module SetTheory where

import BrentHelpers (powerset, removeFirst, groupEq)
import Data.List (foldl', groupBy, sort, sortBy, intercalate)

{- ========================================
    DATA TYPE DEFINITION
======================================== -}
data SetOf a = EmptySet | Set [a] deriving (Eq, Ord)

deSet :: SetOf a -> [a]
deSet (Set s) = s

tseta :: SetOf Integer
tseta = Set [0,1,3,5,7,9]
tsetb :: SetOf Integer
tsetb = Set [0,2,4,6,8]

instance Show a => Show (SetOf a) where
    show :: Show a => SetOf a -> String
    show (Set s) = "{"++ intercalate "," (map show s) ++ "}" 

{- ========================================
    CO-CONSTRUCTOR FUNCTIONS
======================================== -}
-- given a list, make it into a set by ensuring that it has no repeat elements
ensureToSet :: Eq a => [a] -> SetOf a
ensureToSet l
    | null l    = EmptySet
    | otherwise = foldl' addElem (Set []) l

ensureSet :: Eq a => SetOf a -> SetOf a
ensureSet (Set s) = ensureToSet s

isEmpty :: SetOf a -> Bool
isEmpty (Set s) = null s

isElemOf :: Eq a => a -> SetOf a -> Bool
x `isElemOf` (Set s) = x `elem` s

cardinality :: (Eq a, Integral b) => SetOf a -> b
cardinality = fromIntegral . length . deSet . ensureSet

cardinality' :: SetOf a -> Int
cardinality' (Set s) = length s

orderedPair :: Eq a => a -> a -> SetOf (SetOf a)
orderedPair e1 e2 = Set [ensureToSet [e1]] `union` Set [ensureToSet [e1, e2]]

{- ========================================
    ELEMENT OPERATIONS
======================================== -}
-- if x is not in the set, adds it, otherwise returns the set unchanged
addElem :: Eq a => SetOf a -> a -> SetOf a
addElem set@(Set s) x
    | x `isElemOf` set  = set
    | otherwise         = Set (x : s)

-- if x is in the set, removes it, otherwise returns the set unchanged
removeElem :: Eq a => SetOf a -> a -> SetOf a
removeElem set@(Set s) x = Set $ removeFirst s x

{- ========================================
    SET OPERATIONS
======================================== -}
setminus :: Eq a => SetOf a -> SetOf a -> SetOf a
s1@(Set sl1) `setminus` s2@(Set sl2) = foldl' removeElem s1 sl2

union :: Eq a => SetOf a -> SetOf a -> SetOf a
union s1@(Set sl1) s2@(Set sl2) = foldl' addElem s1 sl2

bigUnion :: Eq a => SetOf (SetOf a) -> SetOf a
bigUnion sets@(Set (x:xs)) = foldl' union x xs

intersection :: Eq a => SetOf a -> SetOf a -> SetOf a
intersection (Set []) sety  = Set []
intersection (Set (x:xs)) sety
    | x `isElemOf` sety     = intersection (Set xs) sety `addElem` x
    | otherwise             = intersection (Set xs) sety

bigIntersection :: Eq a => SetOf (SetOf a) -> SetOf a
bigIntersection sets@(Set [])       = Set []
bigIntersection sets@(Set [x])      = x
bigIntersection sets@(Set (x:xs))   = foldl' intersection x xs

{- ========================================
    MAKING NEW SETS
======================================== -}
powerSet :: SetOf a -> SetOf (SetOf a)
powerSet set@(Set s) = makeSetOfSets (powerset s)

filterSet :: (a -> Bool) -> SetOf a -> SetOf a
filterSet f (Set s) = Set $ filter f s

mapSet :: Eq b => (a -> b) -> SetOf a -> SetOf b
mapSet f (Set s) = ensureToSet $ map f s

productSet :: SetOf a -> SetOf b -> SetOf (a,b)
productSet (Set s1) (Set s2) = Set [(x,y) | x <- s1, y <- s2]

productSetSelf :: SetOf a -> SetOf (a, a)
productSetSelf s = productSet s s

makeSetOfSets :: [[a]] -> SetOf (SetOf a)
makeSetOfSets = Set . map Set

{- ========================================
    EQUIVALENCE CLASSES
======================================== -}
-- given some relation 'rel' between elements of a set 's', returns a set of all ordered pairs (x,y) where x `rel` y is True
relationPairs :: (a -> a -> Bool) -> SetOf a -> SetOf (a,a)
relationPairs rel set@(Set s)
    | null s    = Set []
    | otherwise = filterSet (uncurry rel) (productSetSelf set)

-- given some set 's' of ordered pairs, returns a function that determines whether a given pair (x,y) is in that set
-- INVERSE OF 'relationPairs'
makeRelation :: (Eq a, Eq b) => SetOf (a,b) -> (a -> b -> Bool)
makeRelation s x y = (x,y) `isElemOf` s

-- give some equivalence relation 'eq' between elements of a set '(Set s)', returns a set of all equivalence classes of that relation
eqClasses :: Ord a => (a -> a -> Bool) -> SetOf a -> SetOf (SetOf a)
eqClasses eq (Set s) = makeSetOfSets . groupEq eq $ s

eqClassesToRelationPairs :: Eq a => SetOf (SetOf a) -> SetOf (a,a)
eqClassesToRelationPairs set@(Set setl)
    | null setl     = Set []
    | otherwise     = bigUnion $ mapSet (\x -> productSet x x) set

{- ========================================
    CONVERSION BETWEEN SETS & FUNCTIONS
======================================== -}

-- makes a unary function f:a->b from a set of ordered pairs (a,b)
makeFunction1 :: Eq a => SetOf (a,b) -> (a -> b)
makeFunction1 st@(Set s) input
    | null validPairs               = error "makeFunction1 : Function produced does not have the provided argument in its domain."
    | not $ null (tail validPairs)  = error "makeFunction1 : Relation produced is not a valid function (failed vertical line test)."
    | otherwise                     = snd (head validPairs)
    where
        validPairs = filter (\(x,y) -> x==input) s

-- makes a binary function f:a->b->c from a set of ordered pairs (a,b,c)
makeFunction2 :: (Eq a, Eq b) => SetOf (a,b,c) -> (a -> b -> c)
makeFunction2 st@(Set s) inputa inputb
    | null validTrips               = error "makeFunction2 : Function producted does not have a provided argument(s) in its domain."
    | not $ null (tail validTrips)  = error "makeFunction2 : Relation produced is not a valid function (failed vertial line test)."
    | otherwise                     = third (head validTrips)
    where
        third (x,y,z)   = z
        validTrips      = filter (\(x,y,z) -> x==inputa && y==inputb) s