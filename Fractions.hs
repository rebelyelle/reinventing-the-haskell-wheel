{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Fractions where
import qualified GHC.Real (numerator, denominator)
import Data.Char (isDigit)

-- Custom implementation of splitOn function, working with regular strings.
_splitOn :: Char -> String -> [String]
_splitOn c str
    | str == ""         = []
    | c `notElem` str   = [str]
    | otherwise         = str_up_to_c : _splitOn c str_after_c
    where
        str_up_to_c     = fst broken_string
        str_after_c     = tail $ snd broken_string
        broken_string   = break (== c) str

{- ========================================
    DATA TYPE DEFINITION & IMPLEMENTATION
======================================== -}

-- Defines new data type `Fraction` as a pair of integers.
data Fraction = Fraction Integer Integer

-- Makes fractions 'show'able in usual slash-form.
instance Show Fraction where
    show :: Fraction -> String
    show (Fraction n d) = show n ++ "/" ++ show d

-- Not sure if I implemented 'readsPrec' correctly here, but it at least functions to read strings of the expected format like "5/7".
-- NOTE: Does NOT reduce the fraction.
instance Read Fraction where
    readsPrec :: Int -> ReadS Fraction
    readsPrec _ str
        | length split_string > 2   = error "Too many components for Fraction type!"
        | length split_string < 2   = error "Not enough components for Fraction type!"
        | otherwise                 = [(reduceFraction $ Fraction (read numerator_str) (read denominator_str), "")]
        where
            split_string = _splitOn '/' str
            numerator_str = head split_string
            denominator_str = split_string !! 1

-- IMPLEMENTATION: Uses cross-multiplication. Would it be faster to first reduce the fractions, then check if num1==num2 and denom1==denom2?
instance Eq Fraction where
    (==) :: Fraction -> Fraction -> Bool
    (==) (Fraction n1 d1) (Fraction n2 d2) = n1*d2 == n2*d1

-- IMPLEMENTATION: Re-scales both fraction to have common denominator, then compares numerators... Would converting to floats be faster?
instance Ord Fraction where
    (<) :: Fraction -> Fraction -> Bool
    (<) frac1 frac2 = n1s < n2s where
        (Fraction n1s d1s, Fraction n2s d2s) = lcdify frac1 frac2
    (<=) :: Fraction -> Fraction -> Bool
    (<=) frac1 frac2 = (frac1 < frac2) || (frac1 == frac2)

-- IMPLEMENTATION: The arithmetic operators will reduce any resulting fraction.
instance Num Fraction where
    (+) :: Fraction -> Fraction -> Fraction
    (+) (Fraction n1 d1) (Fraction n2 d2) = reduceFraction $ Fraction (n1*d2 + n2*d1) (d1*d2)
    (-) :: Fraction -> Fraction -> Fraction
    (-) (Fraction n1 d1) (Fraction n2 d2) = reduceFraction $ Fraction (n1*d2 - n2*d1) (d1*d2)
    (*) :: Fraction -> Fraction -> Fraction
    (*) (Fraction n1 d1) (Fraction n2 d2) = reduceFraction $ Fraction (n1*n2) (d1*d2)
    abs :: Fraction -> Fraction
    abs (Fraction n d) = reduceFraction $ Fraction (abs n) (abs d)
    signum :: Fraction -> Fraction
    signum (Fraction n d) = Fraction (signum n * signum d) 1
    fromInteger :: Integer -> Fraction
    fromInteger i = Fraction i 1
instance Fractional Fraction where
    (/) :: Fraction -> Fraction -> Fraction
    (/) (Fraction n1 d1) (Fraction n2 d2) = reduceFraction $ Fraction (n1*d2) (n2*d1)
    recip :: Fraction -> Fraction
    recip (Fraction n d) = reduceFraction $ Fraction d n
    fromRational :: Rational -> Fraction
    fromRational r = Fraction (GHC.Real.numerator r) (GHC.Real.denominator r)

{- ========================================
    HELPER FUNCTIONS
======================================== -}
-- grab numerator of Fraction
numerator :: Fraction -> Integer
numerator (Fraction n d) = n
-- grab denominator of Fraction
denominator :: Fraction -> Integer
denominator (Fraction n d) = d

-- reduce fraction to lowest terms, with denominator forced positive
reduceFraction :: Fraction -> Fraction
reduceFraction (Fraction n d)
    | d == 0        = error $ "DIVZERO -- Attempted to reduce " ++ show n ++ "/" ++ show d ++ "."
    | otherwise     = fixNegative $ Fraction (n `div` g) (d `div` g)
    where
        g                           = gcd n d
        fixNegative (Fraction a b)  = Fraction (signum a * signum b * abs a) (abs b)

-- multiply top and bottom of a fraction by a given integer, de-reducing it
scaleFraction :: Integral b => b -> Fraction -> Fraction
scaleFraction s (Fraction n d)
    | s == 0        =   error $ "DIVZERO -- Multiplying numerator and denominator by " ++ show s' ++ " would cause division by 0."
    | otherwise     =   Fraction (s' * n) (s' * d)
    where
        s' = fromIntegral s :: Integer

-- evaluate a fraction as a Float or Double
makeFloating :: RealFrac a => Fraction -> a
makeFloating (Fraction n d)
    | d == 0        = error $ "DIVZERO -- Attempted to convert " ++ show n ++ "/" ++ show d ++ " to floating-point form."
    | otherwise     = nf / df
    where
        nf = fromInteger n
        df = fromInteger d

-- force a Float or Double into Fraction representation _with_ a specified denominator
makeFraction :: (RealFloat a, Integral b) => a -> b -> Fraction
r `makeFraction` d = reduceFraction $ Fraction rd_round d' where
    d'          = fromIntegral d
    rd          = r * fromIntegral d
    rd_round    = round rd

-- given a Fraction, round it to the nearest Fraction with the specified denominator
roundFraction :: Integral b => Fraction -> b -> Fraction
frac `roundFraction` new_d = makeFraction (makeFloating frac) new_d

-- given two fractions, finds their least common positive denominator
lcd :: Fraction -> Fraction -> Integer
lcd (Fraction n1 d1) (Fraction n2 d2) = lcm d1 d2

-- given two fractions, returns a tuple of them with a shared least common positive denominator
lcdify :: Fraction -> Fraction -> (Fraction, Fraction)
lcdify frac1 frac2 = (scaleFraction s1 frac1, scaleFraction s2 frac2) where
    new_d   = lcd frac1 frac2
    s1 = new_d `div` denominator frac1
    s2 = new_d `div` denominator frac2
