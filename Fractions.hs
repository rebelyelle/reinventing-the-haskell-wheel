{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Fractions where

data Fraction = Fraction Integer Integer
instance Show Fraction where
    show :: Fraction -> String
    show (Fraction n d) = show n ++ "/" ++ show d

evalF :: Fraction -> Double
evalF (Fraction n d) = nf / df where
    nf = fromIntegral n :: Double
    df = fromIntegral d :: Double

reduceF :: Fraction -> Fraction
reduceF (Fraction n d) = Fraction (n `div` g) (d `div` g) where
    g = gcd n d

scaleND :: Integer -> Fraction -> Fraction
scaleND s (Fraction n d) = Fraction (s*n) (s*d)

lcd :: Fraction -> Fraction -> Integer
lcd (Fraction n1 d1) (Fraction n2 d2) = lcm d1 d2

addF :: Fraction -> Fraction -> Fraction
addF (Fraction n1 d1) (Fraction n2 d2) = reduceF $ Fraction (n1*d2 + n2*d1) (d1*d2)

subF :: Fraction -> Fraction -> Fraction
subF (Fraction n1 d1) (Fraction n2 d2) = reduceF $ Fraction (n1*d2 - n2*d1) (d1*d2)

mulF :: Fraction -> Fraction -> Fraction
mulF (Fraction n1 d1) (Fraction n2 d2) = reduceF $ Fraction (n1*n2) (d1*d2)

divF :: Fraction -> Fraction -> Fraction
divF (Fraction n1 d1) (Fraction n2 d2) = reduceF $ Fraction (n1*d2) (n2*d1)

pow_Fint :: Fraction -> Integer -> Fraction
pow_Fint (Fraction n d) k = reduceF $ Fraction (n^k) (d^k)

pow_FF :: Fraction -> Fraction -> Double
pow_FF f1 f2 = f1_d ** f2_d where
    f1_d = evalF f1
    f2_d = evalF f2

pow_realF :: Real a => a -> Fraction -> Double
pow_realF r f = r_d ** f_d where
    r_d = realToFrac r :: Double
    f_d = evalF f

pow_Freal :: Real a => Fraction -> a -> Double
pow_Freal f r = f_d ** r_d where
    r_d = realToFrac r :: Double
    f_d = evalF f