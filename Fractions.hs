{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Fractions where
import qualified GHC.Real (numerator, denominator)
import Data.Char (isDigit)
import BrentHelpers ( splitOn )

{- ========================================
    DATA TYPE DEFINITION & IMPLEMENTATION
======================================== -}

-- Defines new data type `Fraction` as a pair of integers.
data FracType a = Fraction { numerator :: a, denominator :: a }

(%) :: a -> a -> FracType a
n % d = Fraction n d

-- Makes fractions 'show'able in usual slash-form.
instance Show a => Show (FracType a) where
    show :: Show a => FracType a -> String
    show (Fraction n d) = "[" ++ show n ++ "] / [" ++ show d ++ "]"

-- Not sure if I implemented 'readsPrec' correctly here, but it at least functions to read strings of the expected format like "5/7".
-- NOTE: Does NOT reduce the fraction.
instance Read a => Read (FracType a) where
    readsPrec :: Int -> ReadS (FracType a)
    readsPrec _ str
        | length split_string > 2   = error "Too many components for Fraction type!"
        | length split_string < 2   = error "Not enough components for Fraction type!"
        | otherwise                 = [(Fraction (read numerator_str) (read denominator_str), "")]
        where
            split_string = splitOn '/' str
            numerator_str = head split_string
            denominator_str = split_string !! 1

-- IMPLEMENTATION: Uses cross-multiplication. Would it be faster to first reduce the fractions, then check if num1==num2 and denom1==denom2?
instance (Num a, Eq a) => Eq (FracType a) where
    (==) :: FracType a -> FracType a -> Bool
    (==) (Fraction n1 d1) (Fraction n2 d2) = n1*d2 == n2*d1

-- IMPLEMENTATION: Re-scales both fraction to have common denominator (product of denominators), then compares numerators... Would converting to floats be faster?
instance (Num a, Ord a) => Ord (FracType a) where
    (<) :: FracType a -> FracType a -> Bool
    (<) frac1 frac2 = n1s < n2s where
        (Fraction n1s d1s, Fraction n2s d2s) = cdify frac1 frac2
    (<=) :: FracType a -> FracType a -> Bool
    (<=) frac1 frac2 = (frac1 < frac2) || (frac1 == frac2)

-- IMPLEMENTATION: The arithmetic operators will reduce any resulting fraction.
instance (Num a) => Num (FracType a) where
    (+) :: FracType a -> FracType a -> FracType a
    (+) (Fraction n1 d1) (Fraction n2 d2) = Fraction (n1*d2 + n2*d1) (d1*d2)
    (-) :: FracType a -> FracType a -> FracType a
    (-) (Fraction n1 d1) (Fraction n2 d2) = Fraction (n1*d2 - n2*d1) (d1*d2)
    (*) :: FracType a -> FracType a -> FracType a
    (*) (Fraction n1 d1) (Fraction n2 d2) = Fraction (n1*n2) (d1*d2)
    abs :: FracType a -> FracType a
    abs (Fraction n d) = Fraction (abs n) (abs d)
    signum :: FracType a -> FracType a
    signum (Fraction n d) = Fraction (signum n * signum d) 1
    fromInteger :: Integer -> FracType a
    fromInteger i = Fraction (fromInteger i) 1
instance Num a => Fractional (FracType a) where
    (/) :: FracType a -> FracType a -> FracType a
    (/) (Fraction n1 d1) (Fraction n2 d2) = Fraction (n1*d2) (n2*d1)
    recip :: FracType a -> FracType a
    recip (Fraction n d) = Fraction d n
    fromRational :: Rational -> FracType a
    fromRational r = Fraction (fromInteger $ GHC.Real.numerator r) (fromInteger $ GHC.Real.denominator r)

{- ========================================
    HELPER FUNCTIONS
======================================== -}
-- reduce fraction to "standard form" : lowest terms, with denominator forced positive
reduceFraction :: (Eq a, Integral a) => FracType a -> FracType a
reduceFraction (Fraction n d)
    | d == 0        = error "DIVZERO -- Attempted to use 'reduceFraction' on a fraction with denominator 0."
    | otherwise     = fixNegativeDenom $ Fraction (n `div` g) (d `div` g) where g = gcd n d

-- force a fraction to have a positive denominator
fixNegativeDenom :: (Num a, Eq a) => FracType a -> FracType a
fixNegativeDenom (Fraction n d)
    | d == 0        = error "DIVZER -- Attempted to use 'fixNegativeDenom' on a fraction with denominator 0."
    | otherwise     = Fraction (signum n * signum d * abs n) (abs d)

-- multiply top and bottom of a fraction by a given a, de-reducing it
scaleFraction :: (Num a, Eq a) => a -> FracType a -> FracType a
scaleFraction s (Fraction n d)
    | s == 0        =   error "DIVZERO -- Attempted to use 'scaleFraction' to make a fraction with denominator 0."
    | otherwise     =   Fraction (s * n) (s * d)

-- evaluate an integer fraction as a Float or Double
makeFloating :: (Integral a, Floating b) => FracType a -> b
makeFloating (Fraction n d)
    | d == 0        = error "DIVZERO -- Attempted to use 'makeFloating' on a fraction with denominator 0."
    | otherwise     = nf / df
    where
        nf = fromIntegral n 
        df = fromIntegral d

-- force a Float or Double into Fraction representation _with_ a specified denominator
makeFraction :: (RealFrac a, Integral b) => a -> b -> FracType b
makeFraction r d = Fraction rd_round d where
    rd          = r * fromIntegral d
    rd_round    = round rd

-- given a Fraction, round it to the nearest Fraction with the specified denominator
roundFraction :: Integral a => FracType a -> a -> FracType a
frac `roundFraction` new_d = makeFraction (makeFloating frac) new_d

-- given two fractions, finds their least common positive denominator
lcd :: Integral a => FracType a -> FracType a -> a
lcd (Fraction n1 d1) (Fraction n2 d2) = lcm d1 d2

-- given two fractions, returns a tuple of them with a shared least common positive denominator
lcdify :: Integral a => FracType a -> FracType a -> (FracType a, FracType a)
lcdify frac1 frac2 = (scaleFraction s1 frac1, scaleFraction s2 frac2) where
    new_d   = lcd frac1 frac2
    s1 = new_d `div` denominator frac1
    s2 = new_d `div` denominator frac2

-- given two fractions, returns a tuple of them both scaled to have the same denominator, defined as the product of the original two denominators (keeping the signs of the original denominators)
cdify :: (Num a, Eq a) => FracType a -> FracType a -> (FracType a, FracType a)
cdify frac1@(Fraction n1 d1) frac2@(Fraction n2 d2) = (scaleFraction (abs d2) frac1, scaleFraction (abs d1) frac2)
