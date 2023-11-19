{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Use (,)" #-}

module Polynomials where
import Data.List (isSuffixOf)

-- helpful typedefs
type Coeff      = Double
type Variable   = Double

{- ========================================
    TYPE DEFINITION & IMPLEMENTATION
======================================== -}
newtype Polynomial = Polynomial [Coeff]
dePolynomial :: Polynomial -> [Coeff]
dePolynomial (Polynomial p) = p

instance Eq Polynomial where
    (==) :: Polynomial -> Polynomial -> Bool
    (==) (Polynomial p1) (Polynomial p2) = p1 == p2
instance Show Polynomial where
    show :: Polynomial -> String
    show poly@(Polynomial p)
        | badEnding `isSuffixOf` usualShow  = sliceEnd (length badEnding) usualShow
        | otherwise                         = usualShow where
            badEnding   = " + 0.0"
            usualShow = defaultShowPolynomial p
            sliceEnd n  = reverse . drop n . reverse
            defaultShowPolynomial lst
                | null lst                = "0.0"
                | length lst == 1         = show thisTerm
                | thisTerm == 0         = defaultShowPolynomial (init lst)
                | otherwise             = showAXN thisTermWithDegree ++ " + " ++ defaultShowPolynomial (init lst) where
                    thisTermWithDegree      = (thisTerm, thisTermDegree)
                    thisTerm                = last lst
                    thisTermDegree          = degree (Polynomial lst)
                    assignPowers lst          = zip lst [0..thisTermDegree]
                    showAXN (a, d)          = show a ++ "*(x^" ++ show d ++ ")"
instance Num Polynomial where
    (+) :: Polynomial -> Polynomial -> Polynomial
    (+) poly1@(Polynomial p1) poly2@(Polynomial p2) = trim0s . Polynomial $ zipWith (+) p1' p2' where
        p1' = dePolynomial $ extendDegree poly1 highestDegree
        p2' = dePolynomial $ extendDegree poly2 highestDegree
        highestDegree = max (degree poly1) (degree poly2)
    (-) :: Polynomial -> Polynomial -> Polynomial
    (-) poly1@(Polynomial p1) poly2@(Polynomial p2) = trim0s . Polynomial $ zipWith (-) p1' p2' where
        p1' = dePolynomial $ extendDegree poly1 highestDegree
        p2' = dePolynomial $ extendDegree poly2 highestDegree
        highestDegree = max (degree poly1) (degree poly2)
    (*) :: Polynomial -> Polynomial -> Polynomial
    (*) poly1@(Polynomial p1) poly2@(Polynomial p2) = trim0s . Polynomial $ map newcoeff [0..newdeg] where
        newdeg = degree poly1 + degree poly2
        newcoeff n = sum $ map (crossterm p1 p2) (termsOfDegree n)
        crossterm a1 a2 (n1, n2)
            | n1 >= length a1    = 0
            | n2 >= length a2    = 0
            | otherwise         = (a1 !! n1) * (a2 !! n2)
        termsOfDegree n = [(x,y) | x <- [0..n], y <- [0..n], x+y==n]
    abs :: Polynomial -> Polynomial
    abs m = error "Cannot take absolute value of a polynomial. Perhaps you meant to use 'degree'?"
    signum :: Polynomial -> Polynomial
    signum m = error "Cannot take signum function of a poylynomial. Perhaps you meant to use 'degree'?"
    fromInteger :: Integer -> Polynomial
    fromInteger i = Polynomial [fromInteger i :: Coeff]

{- ========================================
    BASIC FUNCTIONALITY
======================================== -}
eval :: Polynomial -> Variable -> Variable
eval poly@(Polynomial p) x
    | null p            = 0                                 -- empty polynomials treated as 0
    | otherwise         = a0 + x * eval aLater x where      -- recursive definition: a0 + a1*x + a2*x^2 + ... = a0 + x*(a1 + a2*x + ...)
        a0      = head p
        aLater  = Polynomial (tail p)

degree :: Polynomial -> Int
degree poly@(Polynomial p)
    | null p    = 0
    | p == [0]  = 0
    | otherwise = -1 + length p

{- ========================================
    ADJUSTING LENGTH
======================================== -}

extendDegree :: Polynomial -> Int -> Polynomial
extendDegree poly@(Polynomial p) newDegree
    | degree poly >= newDegree     = poly
    | otherwise                    = Polynomial (p ++ replicate (newDegree - degree poly) 0)

trim0s :: Polynomial -> Polynomial
trim0s poly@(Polynomial p)
    | null p        = poly
    | p == [0]      = poly
    | last p == 0   = trim0s . Polynomial . init $ p
    | otherwise     = poly

{- ========================================
    CALCULUS
======================================== -}
derivative :: Polynomial -> Polynomial
derivative poly@(Polynomial p)
    | length p <= 1     = Polynomial [0]                               -- constant polynomials have derivative 0
    | otherwise         = trim0s . Polynomial $ zipWith (*) (tail p) [1,2..]      -- f(x) = a0 + a1*x + a2*x^2 + a3*x^3 + ... => f'(x) = a1 + 2*a2*x + 3*a3*x^2 + ...

antiderivative :: Variable -> Polynomial -> Polynomial
antiderivative bigC poly@(Polynomial p)
    | null p            = Polynomial [bigC]                            -- zero polynomials have 
    | length p == 1     = trim0s $ Polynomial [bigC, head p]
    | otherwise         = trim0s $ Polynomial (bigC : zipWith (/) p [1,2..])

integrate :: Polynomial -> Variable -> Variable -> Variable
integrate p a b = eval bigP b - eval bigP a where
    bigP = antiderivative 0 p
