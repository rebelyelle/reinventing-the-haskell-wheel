{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module IntegerSequences where
    
import Data.List (foldl')
import BrentHelpers ( combination, neg1power )
import Fractions ( makeFloating, FracType(Fraction) )

{- ================================================
    INTEGER SEQUENCES
================================================ -}
-- Lucas sequences of the first kind
--      P       Q
--      1       -1      Fibonacci numbers
--      2       -1      Pell numbers
--      1       -2      Jacobsthal numbers
--      3       2       numbers of the form (2^n - 1)
--      6       1       square roots of the square triangular numbers
--      2x      1       Chebyshev polynomials of the 2nd kind
--      x+1     x       repunits in base x
lucasSeq1 :: Int -> Int -> Int -> Int
lucasSeq1 _ _ 0 = 0
lucasSeq1 _ _ 1 = 1
lucasSeq1 p q n = p * lucasSeq1 p q (n-1) - q * lucasSeq1 p q (n-2)

seq_Fibonacci :: Int -> Int
seq_Fibonacci = lucasSeq1 1 (-1)
seq_Pell :: Int -> Int
seq_Pell = lucasSeq1 2 (-1)
seq_Jacobsthal :: Int -> Int
seq_Jacobsthal = lucasSeq1 1 (-2)

-- Lucas sequences of the second kind
--      P       Q
--      1       -1      Lucas numbers
--      2       -1      Pell-Lucas numbers
--      1       -2      Jacobsthal-Lucas numbers
--      3       2       numbers of the form (2^n + 1)
--      x       -1      Lucas polynomials
--      2x      1       Chebyshev polynomials of the 1st kind, multiplied by 2
--      x+1     x       numbers of the form (x^n + 1)
lucasSeq2 :: Int -> Int -> Int -> Int
lucasSeq2 _ _ 0 = 2
lucasSeq2 p _ 1 = p
lucasSeq2 p q n = p * lucasSeq2 p q (n-1) - q * lucasSeq2 p q (n-2)

seq_Lucas :: Int -> Int
seq_Lucas = lucasSeq2 1 (-1)
seq_PellLucas :: Int -> Int
seq_PellLucas = lucasSeq2 2 (-1)
seq_JacobsthalLucas :: Int -> Int
seq_JacobsthalLucas = lucasSeq2 1 (-2)

-- Sylvester's sequence
sylvester :: Int -> Int
sylvester 0 = 2
sylvester 1 = 3
sylvester n = 1 + foldl' (*) 1 (map sylvester [0..n-1])

-- Göbel's sequence
gobel :: Int -> Double
gobel 0 = 1.0
gobel n = (1 + sum squares_of_previous_gobels) / fromIntegral n where
    squares_of_previous_gobels = map ((^2) . gobel) [0..n-1]

-- Catalan numbers C(n) := number of ways a product of n+1 numbers can be grouped by associativity
catalan :: Int -> Int
catalan n = ((2*n) `combination` n) `div` (n+1)

-- Bell numbers B(n) := number of ways to partition a set of n elements
bell :: Int -> Int
bell n = sum $ map (stirling2 n) [0..n]

-- Unsigned Stirling number of the first kind {n k} := number of ways to permute n elements with k disjoint cycles
stirling1 :: Int -> Int -> Int
stirling1 n k
    | k > n     = 0
    | n == k    = 1     -- if n == k, then the result is always 1
    | k == 0    = 0     -- if n != 0 and k == 0, then the result is 0
    | otherwise     = (n-1) * stirling1 (n-1) k + stirling1 (n-1) (k-1)

-- Signed Stirling number of the first kind s(n,k) := {n k} * (-1)^(n-k)
stirling1_signed :: Int -> Int -> Int
stirling1_signed n k = neg1power (n - k) * stirling1 n k

-- Stirling number of the second kind [n k] := number of ways to partition a set of n elements into k nonempty subsets
stirling2 :: Int -> Int -> Int
stirling2 n k
    | k > n     = 0
    | n == k    = 1     -- if n == k, then the result is always 1
    | k == 0    = 0     -- if n != 0 and k == 0, then the result is 0
    | k == 1    = 1     -- if n != 0 and k == 1, then the result is 1
    | otherwise     = k * stirling2 (n-1) k + stirling2 (n-1) (k-1)

-- Bernoulli numbers B+(m) -- uses my custom Fractions module to avoid rounding-off errors (from division) until the very end of calculation
bernoulli :: Int -> Double
bernoulli m = makeFloating (foldl' (+) (Fraction 0 1) summands) where
    summands = [bernoullisummand k v m | k <- [0..m], v <- [0..k]]
    bernoullisummand k v m = Fraction summand_numerator summand_denominator where
        summand_numerator       = neg1powerv * kcv * vb1 ^ mb
        summand_denominator     = fromIntegral (k+1) :: Integer
        neg1powerv = fromIntegral (neg1power v) :: Integer
        kcv = fromIntegral (k `combination` v) :: Integer
        vb1 = fromIntegral (v+1) :: Integer
        mb = fromIntegral m :: Integer

-- Eulerian numbers of the 1st kind A(n,k) := number of permutations of numbers 1 thru n, where exactly k elements are greater than the previous element
eulerian1 :: Int -> Int -> Int
eulerian1 n k
    | k == 0        = 1
    | n <= k        = 0
    | k == n-1      = 1
    | otherwise     = (n - k) * eulerian1 (n-1) (k-1) + (k + 1) * eulerian1 (n-1) k

-- Eulerian numbers of the 2nd kind
eulerian2 :: Int -> Int -> Int
eulerian2 n k
    | k == 0        = 1
    | n <= k        = 0
    | otherwise     = (2*n - k - 1) * eulerian2 (n-1) (k-1) + (k + 1) * eulerian2 (n-1) k

-- Euler zigzag numbers A(n)
zigzag :: Int -> Int
zigzag n
    | n < 0         = error "There are no zigzag numbers of negative index."
    | n < 3         = 1
    | otherwise     = sum (map (summation_term (n-1)) [0..n-1]) `div` 2 where
        summation_term n k = combination n k * zigzag k * zigzag (n-k)

-- Euler numbers E(n)
eulerNumber :: Int -> Int
eulerNumber n
    | odd n         = 0
    | otherwise     = neg1power (n `div` 2) * zigzag n

-- Leonardo numbers L(n)
leonardo :: Int -> Int
leonardo n
    | n < 0         = error "There are no Leonardo numbers of negative index."
    | n < 2         = 1
    | otherwise     = leonardo (n-1) + leonardo (n-2) + 1

-- Padovan numbers P(n)
padovan :: Int -> Int
padovan n
    | n < 0         = padovan (n+3) - padovan (n+1) -- from rearrangement of "otherwise" recursion
    | n < 3         = 1
    | otherwise     = padovan (n-2) + padovan (n-3)

-- Perrin numbers P(n)
perrin :: Int -> Int
perrin n
    | n<0           = perrin (n+3) - perrin (n+1) -- from rearrangement of "otherwise" recursion
    | n==0          = 3
    | n==1          = 0
    | n==2          = 2
    | otherwise     = perrin (n-2) + perrin (n-3)

-- Motzkin numbers M(n)
motzkin :: Int -> Int
motzkin n = sum $ map motzkinTerm [0..halfn] where
    halfn = n `div` 2
    motzkinTerm k = catalan k * (n `combination` (2*k))