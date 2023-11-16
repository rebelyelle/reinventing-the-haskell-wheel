{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Use (,)" #-}

module Polynomials where

import Data.List (isSuffixOf)

type Coeff      = Double
type Variable   = Double
type Polynomial = [Coeff]

trim0s :: [Coeff] -> [Coeff]
trim0s p
    | null p        = p
    | p == [0]      = p
    | last p == 0   = trim0s $ init p
    | otherwise     = p

eval :: Polynomial -> Variable -> Variable
eval p x
    | null p            = 0                                 -- empty polynomials treated as 0
    | otherwise         = a0 + x * eval aLater x where      -- recursive definition: a0 + a1*x + a2*x^2 + ... = a0 + x*(a1 + a2*x + ...)
        a0      = head p
        aLater  = tail p

derivative :: Polynomial -> Polynomial
derivative p
    | length p <= 1     = [0]                               -- constant polynomials have derivative 0
    | otherwise         = trim0s $ zipWith (*) (tail p) [1,2..]      -- f(x) = a0 + a1*x + a2*x^2 + a3*x^3 + ... => f'(x) = a1 + 2*a2*x + 3*a3*x^2 + ...

antiderivative :: Variable -> Polynomial -> Polynomial
antiderivative bigC p
    | null p            = [bigC]                            -- zero polynomials have 
    | length p == 1     = trim0s [bigC, head p]
    | otherwise         = trim0s $ bigC : zipWith (/) p [1,2..]

degree :: Polynomial -> Int
degree p
    | null p    = 0
    | p == [0]  = 0
    | otherwise = -1 + length p

integrate :: Polynomial -> Variable -> Variable -> Variable
integrate p a b = eval bigP b - eval bigP a where
    bigP = antiderivative 0 p

add :: Polynomial -> Polynomial -> Polynomial
add p1 p2 = trim0s $ zipWith (+) p1 p2

sub :: Polynomial -> Polynomial -> Polynomial
sub p1 p2 = trim0s $ zipWith (-) p1 p2

mul :: Polynomial -> Polynomial -> Polynomial
mul p1 p2 = trim0s $ map newcoeff [0..newdeg] where
    newdeg = degree p1 + degree p2
    newcoeff n = sum $ map (crossterm p1 p2) (termsOfDegree n)
    crossterm p1 p2 (n1, n2)
        | n1 >= length p1    = 0
        | n2 >= length p2    = 0
        | otherwise         = (p1 !! n1) * (p2 !! n2)
    termsOfDegree n = [(x,y) | x <- [0..n], y <- [0..n], x+y==n]

showPolynomial :: Polynomial -> String
showPolynomial p
    | badEnding `isSuffixOf` usualShow  = sliceEnd (length badEnding) usualShow
    | otherwise                         = usualShow where
        badEnding   = " + 0.0"
        usualShow = defaultShowPolynomial p
        sliceEnd n  = reverse . drop n . reverse
        defaultShowPolynomial p
            | null p                = "0.0"
            | length p == 1         = show thisTerm
            | thisTerm == 0         = defaultShowPolynomial (init p)
            | otherwise             = showAXN thisTermWithDegree ++ " + " ++ defaultShowPolynomial (init p) where
                thisTermWithDegree      = (thisTerm, thisTermDegree)
                thisTerm                = last p
                thisTermDegree          = degree p
                assignPowers p          = zip p [0..thisTermDegree]
                showAXN (a, d)          = show a ++ "*(x^" ++ show d ++ ")"