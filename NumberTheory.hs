{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module NumberTheory where

import Data.List (zip, intersect, foldl', sort)
import Data.Containers.ListUtils (nubOrd)
import Fractions (evalF, Fraction (Fraction), addF)
import Debug.Trace (trace)

{- ================================================
        SET-RELATED FUNCTIONS
================================================ -}

{- BUILT-IN SET OPERATIONS in Data.List:
        - nub           : remove duplicate elements
        - nubOrd        : same as nub, but better optimized for sortable stuff
        - intersect a b : find set intersection (will keep duplicates from a)
        - union a b     : find set union (will keep duplicates from a)
        - a \\ b        : find set difference (elements of 'a' will be deleted only _once_ per occurrence in 'b')
-}

-- given element x and list l, counts number of occurrences of x in l
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-- given a list of elements, calculates the set o fall subsets of that list (order-agnostic)
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs

{- ================================================
        COMBINATIONS & PERMUTATIONS
================================================ -}

-- Kronecker delta δ(i,j) := 1 if i==j, 0 otherwise
kronecker :: (Eq a, Integral b) => a -> a -> b
kronecker i j
        | i==j          = 1
        | otherwise     = 0

levicivita2 :: Integral a => a -> a -> a
levicivita2 i j
        | i==0 && j==1  = 1
        | i==1 && j==0  = -1
        | otherwise     = 0

levicivita3 :: Integral a => a -> a -> a -> a
levicivita3 i j k
        | (i,j,k) `elem` forward_orders         = 1
        | (i,j,k) `elem` backward_orders        = -1
        | otherwise                             = 0
        where
                forward_orders  = [(0,1,2), (1,2,0), (2,0,1)]
                backward_orders = [(2,1,0), (1,0,2), (0,2,1)]

factorial :: Integral a => a -> a
factorial n = product [1..n]

permutation :: Integral a => a -> a -> a
n `permutation` r = product [n-r+1..n]

combination :: Integral a => a -> a -> a
n `combination` r = (n `permutation` r) `div` factorial r

neg1power :: Int -> Int
neg1power n = if even n then 1 else -1


{- ================================================
        NUMBER REPRESENTATION STUFF
================================================ -}
-- given an integer, reverses its digits
reverseInt :: Int -> Int
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
        DIVISIBILITY & FACTORS
================================================ -}
-- tests if n has x as a divisor
divides :: Int -> Int -> Bool
x `divides` n = n `mod` x == 0

-- given an integer, returns its factors w/o duplicates
--IMPLEMENTATION: Factor into primes (w/ duplicates), get the powerset of all primes, get the product of each subset in the powerset to get all factors w/ repeats, then filter out repeats w/ nubOrd
factors :: Int -> [Int]
factors = nubOrd . sort . map product . powerset . primeFactors
--IMPLEMENTATION: Naive, check all numbers less than or equal to n
factors' :: Int -> [Int]
factors' n = filter (`divides` n) [1,2..n]

-- tests if two integers are coprime
coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1

-- returns all numbers from 1 thru n-1 that are coprime to n
totatives :: Int -> [Int]
totatives n = filter (coprime n) [1..n-1]

-- Euler's totient function φ(n) := # of totatives that a number has
-- IMPLEMENTATION: Euler's product formula
totient :: Int -> Int
totient 1 = 1
totient n = n_reduced * product (map (\p -> p-1) (nubOrd $ primeFactors n)) where
        n_reduced  = primeFactorPowers_INV $ map (\(x,y) -> (x,y-1)) (primeFactorPowers n)
-- IMPLEMENTATION: finds all totatives and counts how many there are
totient' :: Int -> Int
totient' n = length (totatives n)

-- returns all common factors of a and b
commonFactors :: Int -> Int -> [Int]
commonFactors a b = factors a `intersect` factors b

-- returns _infinite_ list of all common multiples of a and b
commonMultiples :: Int -> Int -> [Int]
commonMultiples a b = [lcmAB,2*lcmAB..] where
        lcmAB = lcm a b

-- count the number of times that a number f is a factor of n
multiplicityIn :: Int -> Int -> Int
factor `multiplicityIn` 1 = 0
factor `multiplicityIn` n
        | factor > n            = 0
        | factor == n           = 1
        | n `mod` factor == 0   = 1 + factor `multiplicityIn` (n `div` factor)
        | otherwise             = factor `multiplicityIn` (n `div` factor)

-- returns sum of n's proper factors
aliquotsum :: Int -> Int
aliquotsum n = sum (factors n) - n

-- returns difference between sum of n's proper factors and n itself
--      if abundance(n) = 0, n is called "perfect"
--      if abundance(n) > 0, n is called "abundant"
abundance :: Int -> Int
abundance n = aliquotsum n - n

-- returns _infinite_ list of all numbers with more factors than any smaller number
antiprimes :: [Int]
antiprimes = 1 : filter isHighlyComposite [2,4..] where
        isHighlyComposite n     = factorCt n > maximum (map factorCt [1..n-1])
        factorCt                = length . factors

-- returns _infinite_ list of all Square-Free natural numbers:
-- that is, those that do not contain an exponent greater than 1 in their prime factorization
squarefrees :: [Int]
squarefrees = filter (not . hasSquare) [1,2..] where
        hasSquare x = any (`divides` x) (tail $ smallerSquares x)
        smallerSquares x = map (^2) (takeWhile (\k -> k*k <= x) [1..x])

-- determines if a number is Square-Free
isSquareFree :: Int -> Bool
isSquareFree n = n `elem` takeWhile (<= n) squarefrees

-- Fermat's Primality Test: Given a number 'n', we test to see if it __might__ be prime by choosing an arbitrary 'a' and calculating a^(n-1) % n.
--                          If the result is 1, then 'n' might be prime. Otherwise, 'n' cannot be prime.
fermatPrimalityTest :: Int -> Int -> Int
fermatPrimalityTest a n = a ^ (n-1) `mod` n

-- Bézout coefficients: calculate (x,y) such that a*x + b*y = GCD(a,b)
bezout :: Int -> Int -> (Int, Int)
bezout _ 0 = (1,0)
bezout a b = (recursive_y, recursive_x - quotient*recursive_y) where
        quotient                        = a `div` b
        remainder                       = a `mod` b
        (recursive_x, recursive_y)      = bezout b remainder

{- ================================================
        PRIME NUMBER DEFINITIONS & LISTS
        -- WARNING: The lists are infinite, so make sure you only ever grab a range of them
================================================ -}
-- returns an infinite list of all primes
-- IMPLEMENTATION: Base case of 2, then afterward checks only all odd numbers
primes :: [Int]
primes = 2 : filter isPrime [3,5..]

-- returns True if n is prime, False otherwise
-- IMPLEMENTATION: Grabs all primes less than or equal to sqrt(n), and returns True only if all such primes DON'T divide n
--                 Mutually recursive with 'primes' list defined above.
isPrime :: Int -> Bool
isPrime n = not . any (`divides` n) $ smallerPrimes where
        smallerPrimes = takeWhile (\p -> p*p <= n) primes

-- returns an infinite list of all Mersenne primes
-- IMPLEMENTATION: Checks only integers of the form 2^x - 1 for some integer x>1, as all Mersenne primes are of that form.
mersennes :: [Int]
mersennes = filter isMersennePrime powersOf2Minus1 where
        powersOf2Minus1 = map (\x -> 2^x - 1) [2,3..]

-- returns True if n is a Mersenne prime, False otherwise
-- IMPLEMENTATION: a Mersenne prime n (1) is a prime number (2) of the form n = 2^p - 1, p an integer (3) where p is also prime
isMersennePrime :: Int -> Bool
isMersennePrime n = n `elem` primelist && isFloatInteger x && round x `elem` primelist where
        x = logBase 2.0 $ fromIntegral (n + 1)  -- inverse of n = 2^x - 1
        primelist = primesLEQ n

{- ================================================
        PRIME FACTORIZATION
================================================ -}
-- returns the smallest prime factor of n
-- IMPLEMENTATION: If n is prime, return n, otherwise return the first prime that divides n
smallestPrimeFactor :: Int -> Int
smallestPrimeFactor n
        | isPrime n     = n     --after some testing, I've found it _is_ faster to have this guard here
        | n == 1        = error "1 has no prime factors"
        | otherwise     = head . filter (`divides` n) $ primelist
        where   primelist = primesLEQ n

-- returns the smallest prime factor of n
-- IMPLEMENTATION: Calculate all prime factors of n, then grab the last one in the (automatically sorted) list
greatestPrimeFactor :: Int -> Int
greatestPrimeFactor n
        | n == 1        = error "1 has no prime factors"
        | otherwise     = last . primeFactors $ n

-- returns a list of all prime factors of n, including duplicates, automatically in ascending order
primeFactors :: Int -> [Int]
primeFactors n
        | n < 2        = []
        | otherwise     = factor : primeFactors quotient
        where   factor = smallestPrimeFactor n
                quotient = n `div` factor
-- Ω(n) := the number of prime factors of n (with multiplicity-primes counted multiple times)
bigOmega :: Int -> Int
bigOmega = length . primeFactors

-- trims down the list of Prime Factors to just those unique numbers
primeFactorsUnique :: Int -> [Int]
primeFactorsUnique = nubOrd . primeFactors

-- ω(n) := the number of unique prime factors of n
smallOmega :: Int -> Int
smallOmega = length . primeFactorsUnique

-- return a list of tuples of n's prime factors (no duplicates) and their prime multiplicity in n
primeFactorPowers :: Int -> [(Int, Int)]
primeFactorPowers 1 = []
primeFactorPowers n = zip ps rs where
        ps = nubOrd . primeFactors $ n             -- prime factors of n, w/o duplicates
        rs = map (`multiplicityIn` n) ps            -- exponents of the primes in "rs"

-- inverse function on primeFactorPowers
primeFactorPowers_INV :: [(Int,Int)] -> Int
primeFactorPowers_INV [] = 1
primeFactorPowers_INV ((p,n):xs) = p ^ n * primeFactorPowers_INV xs

{- ================================================
        PRIME-COUNTING & RELATED FUNCTIONS
================================================ -}

-- prime-counting function π(n) := the number of primes less than or equal to n
primecount :: Int -> Int
primecount n
        | n < 2         = 0
        | otherwise     = length (takeWhile (<= n) primes)

-- returns the list of all primes less than or equal to n
primesLEQ :: Int -> [Int]
primesLEQ n = takeWhile (<= n) primes

-- get all (p,k) pairs such that p is prime, k is a natural number, and p^k <= n
primePowersLEQ :: Int -> [(Int, Int)]
primePowersLEQ n = [(fromIntegral p, fromIntegral k) | p <- bigint_pleqn, k <- [1..bigint_n], (p^k) <= bigint_n] where
        bigint_pleqn = map fromIntegral (primesLEQ n)
        bigint_n = fromIntegral n :: Integer

-- Liouville function λ(n) := (-1) ^ Ω(n)
liouville :: Int -> Int
liouville n = if even (bigOmega n) then 1 else -1

-- Möbius function μ(n) := the sum of the primitive nth roots of unity
mobius :: Int -> Int
mobius n = kronecker (smallOmega n) (bigOmega n) * liouville n
-- naive implementation, requires (more costly) determination if n is square-free or not
mobius' :: Int -> Int
mobius' n
        | isSquareFree n        = if even (bigOmega n) then 1 else -1
        | otherwise             = 0

-- Mertens function M(n) := sum of μ(i) from i=1 to n
mertens :: Int -> Int
mertens n = sum $ map mobius [1..n]

-- Primorial function n# := product of all primes less than or equal to n
primorial :: Integral a => Int -> a
primorial 0 = 1
primorial 1 = 1
primorial n = product $ map fromIntegral (primesLEQ n)

-- first Chebyshev function ϑ(n) := sum of natural log of all primes less than or equal to n
-- IMPLEMENTATION: faster & more accurate, but returns Infinity for n > 102
chebyshev1 :: Int -> Double
chebyshev1 = log . fromIntegral . primorial  --uses the fact that ln(a*b) = ln(a) + ln(b)
-- IMPLEMENTATION: slower & less accurate, but returns a number for all n tested (up to 500,000)
chebyshev1' :: Int -> Double
chebyshev1' n = sum $ map (log . fromIntegral) (primesLEQ n)

-- second Chebyshev function ψ(n) := sum of natural log of all p^k where p is prime and k is a natural number
-- IMPLEMENTATION: faster, unsure abouta ccuracy
chebyshev2 :: Int -> Double
chebyshev2 n = sum $ map vonMangoldt [1..n]
-- IMPLEMENTATION:
chebyshev2' :: Int -> Double
chebyshev2' n = log . fromIntegral $ product (chebyshevSetup2 n) where
        chebyshevSetup2 n = map (fromIntegral . fst) (primePowersLEQ n)

-- Von Mangoldt function Λ(n) := log p (if n = p^k for some prime p and k>= 1), else 0
vonMangoldt :: Int -> Double
vonMangoldt n
        | length primeFactorPowers_n == 1       = log (fromIntegral p)
        | otherwise                             = 0
        where   primeFactorPowers_n = primeFactorPowers n
                (p,k) = head primeFactorPowers_n


{- ================================================
        PRIME-LIKE & PRIME-RELATED PROPERTIES
================================================ -}
-- a number is "k-almost prime" if it is the product of exactly k prime numbers
isAlmostPrime :: Int -> Int -> Bool
isAlmostPrime k n = length (primeFactors n) == k

-- a number is considered "semiprime" if it is the product of exactly 2 prime numbers
isSemiPrime :: Int -> Bool
isSemiPrime = isAlmostPrime 2

-- a Carmichael number is: (1) composite, (2) square-free, (3) p|n => (p-1)|(n-1).
isCarmichael :: Int -> Bool
isCarmichael n = isSquareFree n && and all_korselts && not (isPrime n) where
        all_korselts            = map (`korselt_condition` n) (primeFactorsUnique n)
        korselt_condition p n   = p `divides` n && (p-1) `divides` (n-1)

-- a Lucas-Carmichael number is: (1) composite, (2) square-free, (3) p|n => (p+1)|(n+1), and (4) odd.
isLucasCarmichael :: Int -> Bool
isLucasCarmichael n = isSquareFree n && and all_conditions && odd n && not (isPrime n) where
        all_conditions          = map (`condition` n) (primeFactorsUnique n)
        condition p n           = p `divides` n && (p+1) `divides` (n+1)

-- a Størmer number is: (1) positive, and (2) has n^2 + 1 >= 2n.
isStrormer :: Int -> Bool
isStrormer n = greatestPrimeFactor (n*n + 1) >= (2*n)

-- a number is "pseudoprime to base b" if it passes Fermat's primality test base 'b', but it is not actually prime
isPseudoPrime :: Int -> Int -> Bool
isPseudoPrime base n = fermatPrimalityTest base n == 1 && not (isPrime n)

-- a number is "perfect" if all of its proper factors sum up to the number itself
isPerfect :: Int -> Bool
isPerfect n = abundance n == 0

-- a number is "abundant" if all of its proper factors sum up to greater than the number itself
isAbundant :: Int -> Bool
isAbundant n = abundance n > 0

isDeficient :: Int -> Bool
isDeficient n = abundance n < 0

-- a number is "semiperfect" if some subset of its proper divisors sums up to the number itself
isSemiPerfect :: Int -> Bool
isSemiPerfect n = n `elem` sums_of_subsets_of_divisors where
        sums_of_subsets_of_divisors = map sum (powerset proper_factors_of_n)
        proper_factors_of_n = init (factors n)

-- a number is "weird" if it is abundant but not semiperfect.
isWeird :: Int -> Bool
isWeird n = n_div_70 `elem` valid_primes && (n `mod` 70 == 0) || isAbundant n && not (isSemiPerfect n) where
        valid_primes = filter (>= 149) . takeWhile (<= n_div_70) $ primes
        n_div_70 = n `div` 70

-- a number is "k-smooth" if all of its factors are less than or equal to k.
isSmooth :: Int -> Int -> Bool
isSmooth k n = all (<= k) (factors n)

-- a number is "k-rough" if all of its factors are greater than or equal to k.
isRough :: Int -> Int -> Bool
isRough k n = all (>= k) (factors n)

-- a number n is "practical" if all smaller positive integers can be expressed as sums of distinct divisors of n.
isPractical :: Int -> Bool
isPractical n = all (`isSumOfSomeSubsetOf` factors n) [1..n-1] where
        k `isSumOfSomeSubsetOf` someList = k `elem` (map sum . powerset) someList

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
        | k > n         = 0
        | n == k        = 1     -- if n == k, then the result is always 1
        | k == 0        = 0     -- if n != 0 and k == 0, then the result is 0
        | otherwise     = (n-1) * stirling1 (n-1) k + stirling1 (n-1) (k-1)

-- Signed Stirling number of the first kind s(n,k) := {n k} * (-1)^(n-k)
stirling1_signed :: Int -> Int -> Int
stirling1_signed n k = neg1power (n - k) * stirling1 n k

-- Stirling number of the second kind [n k] := number of ways to partition a set of n elements into k nonempty subsets
stirling2 :: Int -> Int -> Int
stirling2 n k
        | k > n         = 0
        | n == k        = 1     -- if n == k, then the result is always 1
        | k == 0        = 0     -- if n != 0 and k == 0, then the result is 0
        | k == 1        = 1     -- if n != 0 and k == 1, then the result is 1
        | otherwise     = k * stirling2 (n-1) k + stirling2 (n-1) (k-1)

-- Bernoulli numbers B+(m) -- uses my custom Fractions module to avoid rounding-off errors (from division) until the very end of calculation
bernoulli :: Int -> Double
bernoulli m = evalF $ foldl' addF (Fraction 0 1) summands where
        summands = [bernoullisummand k v m | k <- [0..m], v <- [0..k]]
        bernoullisummand k v m = Fraction summand_numerator summand_denominator where
                summand_numerator       = neg1powerv * kcv * (vb1 ^ mb)
                summand_denominator     = fromIntegral (k+1) :: Integer
                neg1powerv = fromIntegral (neg1power v) :: Integer
                kcv = fromIntegral (k `combination` v) :: Integer
                vb1 = fromIntegral (v+1) :: Integer
                mb = fromIntegral m :: Integer