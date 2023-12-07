{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module NumberTheory where

import Data.List (zip, intersect, foldl', sort)
import Data.Containers.ListUtils (nubOrd)
import Fractions (makeFloating, FracType (Fraction))
import Debug.Trace (trace)
import BrentHelpers ( powerset, kronecker, combination, neg1power, isFloatInteger )

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

totientSummary :: Int -> Int
totientSummary n = sum $ map totient [1..n]

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

-- sum-of-divisors function σ_z(n)
sumDivisorPower :: Integral p => p -> Int -> Int
sumDivisorPower z n
    | z == 0                = length theFactors
    | z == 1                = sum theFactors
    | otherwise             = sum $ map (^z) theFactors
    where
        theFactors = factors n

-- returns sum of n's proper factors
aliquotsum :: Int -> Int
aliquotsum n = sumDivisorPower 1 n - n

-- returns difference between sum of n's proper factors and n itself
--      if abundance(n) = 0, n is called "perfect"
--      if abundance(n) > 0, n is called "abundant"
abundance :: Int -> Int
abundance n = aliquotsum n - n

abundancyIndex :: Int -> Double
abundancyIndex n = fromIntegral (abundance n) / fromIntegral n

-- returns _infinite_ list of all numbers with more factors than any smaller number
antiprimes :: [Int]
antiprimes = 1 : filter isHighlyComposite [2,4..] where
    isHighlyComposite n     = factorCt n > maximum (map factorCt [1..n-1])
    factorCt        = length . factors

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
--              If the result is 1, then 'n' might be prime. Otherwise, 'n' cannot be prime.
fermatPrimalityTest :: Int -> Int -> Int
fermatPrimalityTest a n = a ^ (n-1) `mod` n

-- Bézout coefficients: calculate (x,y) such that a*x + b*y = GCD(a,b)
bezout :: Int -> Int -> (Int, Int)
bezout _ 0 = (1,0)
bezout a b = (recursive_y, recursive_x - quotient*recursive_y) where
    quotient            = a `div` b
    remainder               = a `mod` b
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
--         Mutually recursive with 'primes' list defined above.
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
    | n == 1    = error "1 has no prime factors"
    | otherwise     = head . filter (`divides` n) $ primelist
    where   primelist = primesLEQ n

-- returns the smallest prime factor of n
-- IMPLEMENTATION: Calculate all prime factors of n, then grab the last one in the (automatically sorted) list
greatestPrimeFactor :: Int -> Int
greatestPrimeFactor n
    | n == 1    = error "1 has no prime factors"
    | otherwise     = last . primeFactors $ n

-- returns a list of all prime factors of n, including duplicates, automatically in ascending order
primeFactors :: Int -> [Int]
primeFactors n
    | n < 2    = []
    | otherwise     = factor : primeFactors quotient
    where
        factor = smallestPrimeFactor n
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
    ps = nubOrd . primeFactors $ n         -- prime factors of n, w/o duplicates
    rs = map (`multiplicityIn` n) ps        -- exponents of the primes in "rs"

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

-- returns a 2-tuple with lower and upper bounds for where the nth prime is located (inclusive). The formula is only valid for n>=6, so an ad-hoc solution by me is provided in its place.
boundsNthPrime :: Int -> (Int, Int)
boundsNthPrime n
    | n < 6         = (n, 2*n+1)
    | otherwise     = (minBound, maxBound)
    where
        minBound  = if even minBound' then minBound' + 1 else minBound'
        maxBound  = if even maxBound' then maxBound' - 1 else maxBound'
        minBound' = ceiling $ fn * (log fn + log (log fn) - 1)
        maxBound' = floor $ fn * (log fn + log (log fn))
        fn = fromIntegral n :: Double

-- returns a list of 
rangeNthPrime :: Int -> [Int]
rangeNthPrime n = [minBound..maxBound] where
    (minBound, maxBound) = boundsNthPrime n

-- returns the list of all primes less than or equal to n
primesLEQ :: Int -> [Int]
primesLEQ n = takeWhile (<= n) primes

-- get all (p,k) pairs such that p is prime, k is a natural number, and p^k <= n
primePowersLEQ :: Int -> [(Int, Int)]
primePowersLEQ n = [(fromIntegral p, fromIntegral k) | p <- bigint_pleqn, k <- [1..bigint_n], p^k <= bigint_n] where
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
    | isSquareFree n    = if even (bigOmega n) then 1 else -1
    | otherwise         = 0

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
    | otherwise                 = 0
    where
        primeFactorPowers_n = primeFactorPowers n
        (p,k) = head primeFactorPowers_n

{-
-- TODO
isQuadraticResidue :: Int -> Int -> Bool
isQuadraticResidue q n = (q `mod` n) `elem` squares where
    squares = map ((`mod` n) . (^2)) [0..TODO]
-}

legendreSymbol :: Int -> Int -> Int
legendreSymbol a p
    | p==2                      = error "legendreSymbol: Cannot calculate Legendre symbol with 2 as the prime (second argument)."
    | p `notElem` primelist     = error $ "legendreSymbol: " ++ show p ++ " is not a prime number."
    | otherwise                 = fromInteger (fix result)
    where
        primelist   = primesLEQ p
        fix r       = if r > 1 then -1 else r
        result      = a' ^ ((p'-1) `div` 2) `mod` p'
        a'          = fromIntegral a :: Integer
        p'          = fromIntegral p :: Integer

legendreSymbol' :: Int -> Int -> Int
legendreSymbol' a p
    | p == 0                    = if a == 1 || a == -1 then 1 else 0
    | p == 1                    = 1
    | p == 2                    =
        if even a
        then 0
        else if a `mod` 8 == 1 || a `mod` 8 == 7 then 1 else -1
    | p == -1                   = if a < 0 then -1 else 1
    | p `notElem` primelist     = error $ "legendreSymbol': " ++ show p ++ " is not a valid input number."
    | otherwise                 = fromInteger (fix result)
    where
        primelist   = primesLEQ p
        fix r       = if r > 1 then -1 else r
        result      = a' ^ ((p'-1) `div` 2) `mod` p'
        a'          = fromIntegral a :: Integer
        p'          = fromIntegral p :: Integer

jacobiSymbol :: Int -> Int -> Int
jacobiSymbol a n
    | even n    = error "jacobiSymbol: Cannot calculate Jacobi symbol with an even number as the second argument."
    | otherwise = product $ map (\(p,k) -> legendreSymbol a p ^ k) (primeFactorPowers n)

jacobiSymbol' :: Int -> Int -> Int
jacobiSymbol' a n = product $ map (\(p,k) -> legendreSymbol' a p ^ k) (primeFactorPowers n)

--TODO
{-
kroneckerSymbol :: Int -> Int -> Int
kroneckerSymbol a n
    | otherwise     = legendreSymbol' a u * jacobiSymbol' a (abs n)
    where
        u = signum n
-}

adicValuation :: Int -> Int -> Int
p `adicValuation` n
    | p `elem` primeFactors n   = p `multiplicityIn` n
    | otherwise                 = 0

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
    all_korselts        = map (`korselt_condition` n) (primeFactorsUnique n)
    korselt_condition p n   = p `divides` n && (p-1) `divides` (n-1)

-- a Lucas-Carmichael number is: (1) composite, (2) square-free, (3) p|n => (p+1)|(n+1), and (4) odd.
isLucasCarmichael :: Int -> Bool
isLucasCarmichael n = isSquareFree n && and all_conditions && odd n && not (isPrime n) where
    all_conditions      = map (`condition` n) (primeFactorsUnique n)
    condition p n       = p `divides` n && (p+1) `divides` (n+1)

-- a Størmer number is: (1) positive, and (2) has n^2 + 1 >= 2n.
isStrormer :: Int -> Bool
isStrormer n = greatestPrimeFactor (n*n + 1) >= 2*n

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
isWeird n = n_div_70 `elem` valid_primes && n `mod` 70 == 0 || isAbundant n && not (isSemiPerfect n) where
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
