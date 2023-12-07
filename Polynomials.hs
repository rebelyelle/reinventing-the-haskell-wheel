{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Use (,)" #-}
{-# HLINT ignore "Use camelCase" #-}

module Polynomials where
import Data.List ( isSuffixOf )
import BrentHelpers
    ( alternateSigns, combination, factorial, isFinite, neg1power )
import Fractions ( FracType(..), (%) )
import IntegerSequences (stirling2)

{- ========================================
    DEFINE TYPECLASS AND OPERATIONS FOR POLYNOMIAL TYPES
======================================== -}
type CoeffVar       = Double

class (Eq a, Num a) => RealFunction a where
    eval :: a -> CoeffVar -> CoeffVar       -- evaluate the function f(x) at some x
    fromReal :: Real r => r -> a            -- convert a real number into a constant function                 
    shiftH :: a -> CoeffVar -> a            -- shift f(x) left to f(x+c)
    scaleH :: a -> CoeffVar -> a            -- scale f(x) horizontally to f(c*x)
    flipH :: a -> a                         -- flip f(x) horizontally to f(-x)
    derivative :: a -> a                    -- derivative, calculated analytically
    antiderivative :: CoeffVar -> a -> a    -- antiderivative, calculated analytically; constant C is given
    antiderivative x f = error "Antiderivative not implemented for this type."
    {-# MINIMAL eval, fromReal, derivative, shiftH, flipH, scaleH #-}

limits :: RealFunction a => a -> CoeffVar -> (CoeffVar, CoeffVar)
limits f c
    | isFinite (abs (eval f c)) = (value, value)
    | otherwise                 = (eval f (c-epsilon), eval f (c+epsilon))
    where
        value = eval f c
        epsilon = 0.0000000001

limit :: RealFunction a => a -> CoeffVar -> CoeffVar
limit f c
    | abs (rightLim - leftLim) < epsilon*2  = (rightLim + leftLim)/2
    | otherwise                             = error $ "Limit does not exist! One-sided limits: " ++ show leftLim ++ ", " ++ show rightLim
    where
        (leftLim, rightLim) = limits f c
        epsilon = 0.0001

shiftV :: RealFunction a => a -> CoeffVar -> a            -- shift f(x) upward to f(x)+c
shiftV f c = f + fromReal c
scaleV :: RealFunction a => a -> CoeffVar -> a            -- scale f(x) vertically to c*f(x)
scaleV f c = f * fromReal c
flipV :: RealFunction a => a -> a                         -- flip f(x) vertically to -f(x)
flipV = negate

-- proper integral ∫ f(x) dx from a to b (assumes closed interval [a,b] is in domain of f)
integrate :: RealFunction a => a -> CoeffVar -> CoeffVar -> CoeffVar
integrate f a b = eval bigF b - eval bigF a
    where bigF = antiderivative 0 f

-- estimation of integral ∫ f(x) dx from a to b using trapezoid rule (assumes closed interval [a,b] is in domain of f)
integrate' :: RealFunction a => a -> CoeffVar -> CoeffVar -> CoeffVar
integrate' p a b = delta_x * sum (zipWith trapezoidal allintervals (tail allintervals))
    where
        delta_x = (b - a) / numIntervals
        allintervals = map (eval p) [a,(a + delta_x)..b]
        trapezoidal x y = (x+y)/2
        numIntervals = 100000 :: CoeffVar

-- forward difference operator f(x) -> f(x+1) - f(x)
forwardDiff :: RealFunction a => a -> a
forwardDiff f = shiftH f 1 - f

-- backward difference operator f(x) -> f(x) - f(x-1)
backwardDiff :: RealFunction a => a -> a
backwardDiff f = f - shiftH f (-1)

-- central difference operator f(x) -> f(x+½) - f(x-½)
centralDiff :: RealFunction a => a -> a
centralDiff f = shiftH f 0.5 - shiftH f (-0.5)

--apply an operator like d/dx multiple times
multiapply :: (Integral i, RealFunction a) => i -> (a -> a) -> a -> a
multiapply 0 op = id
multiapply n op = op . multiapply (n-1) op

--determine if function is "even": flipping horizontally leaves function unchanged
isEven :: RealFunction a => a -> Bool
isEven f = flipH f == f

--determine is function is "odd": flipping horizontally is same as flipping vertically
isOdd :: RealFunction a => a -> Bool
isOdd f = flipH f == flipV f

-- uses Newton's method to find a root of a function near a given guess
rootNear :: RealFunction a =>  CoeffVar -> a -> CoeffVar
rootNear guess f = limitedRootSearch guess f 0 where
    limitedRootSearch guess f n
        | n > limit                         = error $ "Cannot find root near given guess! (Tried " ++ show limit ++ " iterations of Newton's Method.)"
        | abs (eval f' guess) < epsilon  = error $ "rootnear -- Newton's method failed--derivative of function at x=" ++ show guess ++ " is too small!"
        | abs (eval f guess) < epsilon   = guess
        | otherwise                         = limitedRootSearch newguess f (n+1)
        where
            newguess    = guess - eval f guess / eval f' guess
            f'       = derivative f
            epsilon     = 0.0000000001
            limit       = 1000

{- ========================================
    POLYNOMIAL TYPE
======================================== -}
newrype PolynomType     = Polynomial [CoeffVar]

dePolynomial :: PolynomType -> [CoeffVar]
dePolynomial (Polynomial p) = p

instance Eq PolynomType where
    (==) :: PolynomType -> PolynomType -> Bool
    (==) (Polynomial p1) (Polynomial p2) = p1 == p2
-- ordering first prioritizes degree, then
instance Ord PolynomType where
    (<=) :: PolynomType -> PolynomType -> Bool
    (<=) poly1 poly2 = poly1 < poly2 || poly1 == poly2
    (<) :: PolynomType -> PolynomType -> Bool
    (<) poly1@(Polynomial p1) poly2@(Polynomial p2)
        | isZeroPolynomial poly1 && isZeroPolynomial poly2  = False
        | degree' poly1 /= degree' poly2                    = degree' poly1 < degree' poly2
        | last p1 /= last p2 && last p1 == 0                = True
        | last p1 /= last p2 && last p2 == 0                = False
        | last p1 /= last p2                                = last p1 < last p2
        | otherwise                                         = Polynomial (init p1) < Polynomial (init p2)

instance Show PolynomType where
    show :: PolynomType -> String
    show poly@(Polynomial p)
        | badEnding `isSuffixOf` usualShow  = sliceEnd (length badEnding) usualShow
        | otherwise                         = usualShow
        where
            badEnding   = " + 0.0"
            usualShow   = defaultShowPolynomial p
            sliceEnd n  = reverse . drop n . reverse
            defaultShowPolynomial lst
                | null lst              = "0.0"
                | length lst == 1       = show thisTerm
                | thisTerm == 0         = defaultShowPolynomial (init lst)
                | otherwise             = showAXN thisTermWithDegree ++ " + " ++ defaultShowPolynomial (init lst)
                where
                    thisTermWithDegree      = (thisTerm, thisTermDegree)
                    thisTerm                = last lst
                    thisTermDegree          = degree (Polynomial lst)
                    assignPowers lst        = zip lst [0..thisTermDegree]
                    showAXN (a, d)          = show a ++ "*(x^" ++ show d ++ ")"
instance Num PolynomType where
    (+) :: PolynomType -> PolynomType -> PolynomType
    (+) poly1@(Polynomial p1) poly2@(Polynomial p2) = trim0s . Polynomial $ zipWith (+) p1' p2' where
        p1' = dePolynomial $ extendDegree poly1 highestDegree
        p2' = dePolynomial $ extendDegree poly2 highestDegree
        highestDegree = max (degree poly1) (degree poly2)
    (-) :: PolynomType -> PolynomType -> PolynomType
    (-) poly1@(Polynomial p1) poly2@(Polynomial p2) = trim0s . Polynomial $ zipWith (-) p1' p2' where
        p1' = dePolynomial $ extendDegree poly1 highestDegree
        p2' = dePolynomial $ extendDegree poly2 highestDegree
        highestDegree = max (degree poly1) (degree poly2)
    (*) :: PolynomType -> PolynomType -> PolynomType
    (*) poly1@(Polynomial p1) poly2@(Polynomial p2) = trim0s . Polynomial $ map newcoeff [0..newdeg] where
        newdeg = degree poly1 + degree poly2
        newcoeff n = sum $ map (crossterm p1 p2) (termsOfDegree n)
        crossterm a1 a2 (n1, n2)
            | n1 >= length a1    = 0
            | n2 >= length a2    = 0
            | otherwise         = (a1 !! n1) * (a2 !! n2)
        termsOfDegree n = [(x,y) | x <- [0..n], y <- [0..n], x+y==n]
    abs :: PolynomType -> PolynomType
    abs poly = if last p' < 0 then negate poly else poly
        where p' = dePolynomial (trim0s poly)
    signum :: PolynomType -> PolynomType
    signum poly
        | isZeroPolynomial poly = Polynomial [0]
        | last p' > 0           = Polynomial [1]
        | otherwise             = Polynomial [-1]
        where p' = dePolynomial (trim0s poly)
    fromInteger :: Integer -> PolynomType
    fromInteger = fromReal

instance RealFunction PolynomType where
    eval :: PolynomType -> CoeffVar -> CoeffVar
    eval poly@(Polynomial p) x
        | null p            = 0                                 -- empty polynomials treated as 0
        | x == 0            = a0
        | otherwise         = a0 + x * eval aLater x where      -- recursive definition: a0 + a1*x + a2*x^2 + ... = a0 + x*(a1 + a2*x + ...)
            a0      = head p
            aLater  = Polynomial (tail p)
    shiftH :: PolynomType -> CoeffVar -> PolynomType
    shiftH poly@(Polynomial p) plusc = sum $ map (`shiftTerm` plusc) (assignPowers poly) where
        shiftTerm (coeff, pow) plusc = Polynomial $ map kthTerm [0..pow] where
            kthTerm k = coeff * fromIntegral (pow `combination` k) * plusc ^ (pow - k)
    scaleH :: PolynomType -> CoeffVar -> PolynomType
    scaleH poly@(Polynomial p) timesc = Polynomial $ zipWith (\n a -> a * (timesc^n)) [0,1..(length p)] p
    flipH :: PolynomType -> PolynomType
    flipH (Polynomial p) = Polynomial (alternateSigns p)
    derivative :: PolynomType -> PolynomType
    derivative poly@(Polynomial p)
        | length p <= 1     = Polynomial [0]                               -- constant polynomials have derivative 0
        | otherwise         = trim0s . Polynomial $ zipWith (*) (tail p) [1,2..]      -- f(x) = a0 + a1*x + a2*x^2 + a3*x^3 + ... => f'(x) = a1 + 2*a2*x + 3*a3*x^2 + ...
    antiderivative :: CoeffVar -> PolynomType -> PolynomType
    antiderivative bigC poly@(Polynomial p)
        | null p            = Polynomial [bigC]                            -- zero polynomials have 
        | length p == 1     = trim0s $ Polynomial [bigC, head p]
        | otherwise         = trim0s $ Polynomial (bigC : zipWith (/) p [1,2..])
    fromReal :: Real r => r -> PolynomType
    fromReal x = Polynomial [realToFrac x]

instance Enum PolynomType where
    toEnum :: Int -> PolynomType
    toEnum = fromIntegral
    fromEnum :: PolynomType -> Int
    fromEnum p = error "Do not use 'fromEnum' on Polynomial types."

instance Real PolynomType where
    toRational :: PolynomType -> Rational
    toRational p = error "Do not use 'toRational' on Polynomial types."

instance Integral PolynomType where
    div :: PolynomType -> PolynomType -> PolynomType
    div a@(Polynomial p1) b@(Polynomial p2)
        | null p2 || head p2 == 0   = error "Cannot divide by the zero polynomial!"
        | degree a < degree b       = Polynomial [0]
        | degree b == 0             = Polynomial $ map (/ head p2) p1
        | otherwise                 = s + div (a - s*b) b
        where
            s = Polynomial $ replicate s_power 0 ++ [s_coeff]
            s_coeff = last p1 / last p2
            s_power = degree a - degree b
    quot :: PolynomType -> PolynomType -> PolynomType
    quot = div
    rem :: PolynomType -> PolynomType -> PolynomType
    rem a b = a - (b * (a `div` b))
    quotRem :: PolynomType -> PolynomType -> (PolynomType, PolynomType)
    quotRem a b = (div a b, rem a b)
    toInteger :: PolynomType -> Integer
    toInteger p = error "Do not use 'toInteger' on Polynomial types."


-- polynomial Euclidean algorithm
gcdP :: PolynomType -> PolynomType -> PolynomType
gcdP p1 p2
    | isZeroPolynomial p2   = Polynomial [0]
    | isZeroPolynomial p1   = p2
    | degree p1 == 0        = Polynomial [1]
    | otherwise             = gcdP (a-b) b
    where
        a = max p1 p2
        b = min p1 p2

-- given a polynomial of degree n, and an integer newDegree > n, extends the polynomial's underlying list representation to "degree" newDegree by appending 0-coefficients
extendDegree :: PolynomType -> Int -> PolynomType
extendDegree poly@(Polynomial p) newDegree
    | degree poly >= newDegree     = poly
    | otherwise                    = Polynomial (p ++ replicate (newDegree - degree poly) 0)

-- given a polynomial, trims all trailing 0s from its underlying list representation
trim0s :: PolynomType -> PolynomType
trim0s poly@(Polynomial p)
    | null p        = poly
    | p == [0]      = poly
    | last p == 0   = trim0s . Polynomial . init $ p
    | otherwise     = poly

-- determines if a polynomial is equivalent to the zero polynomial p(x) = 0
isZeroPolynomial :: PolynomType -> Bool
isZeroPolynomial = null . dePolynomial . trim0s

-- determines if a polynomial is a constant polynomial p(x) = a0
isConstant :: PolynomType -> Bool
isConstant (Polynomial p) = length p < 2

-- calculate degree of polynomial, the highest power of x with nonzero coefficient to appear in the formula for p(x)
degree :: PolynomType -> Int
degree = length_minus_1 . trim0s where
    length_minus_1 (Polynomial p)
        | null p    = 0
        | otherwise = -1 + length p

-- same as above, except that the zero polynomial p(x) = 0 is defined to have degree "-1"
degree' :: PolynomType -> Int
degree' p
    | isZeroPolynomial p    = -1
    | otherwise             = degree p

-- given a polynomial a0 + a1*x^1 + ... + an*x^n, creates a list [(a0,0), (a1,1), ..., (an,n)]
assignPowers :: Integral a => PolynomType -> [(CoeffVar, a)]
assignPowers (Polynomial p) = zip p [0,1..]

-- calculates the exact roots of a polynomial (only degree 0, 1, and 2 for now)
rootsPoly :: PolynomType -> [CoeffVar]
rootsPoly poly = case trim0s poly of
    Polynomial []           -> error "A degree-0 polynomial has either no roots or infinite roots."
    Polynomial [a]          -> error "A degree-0 polynomial has either no roots or infinite roots."
    Polynomial [a,b]        -> [-b / a]
    Polynomial [a,b,c]      ->
        if discriminant > 0
        then [ (-b + sqrt discriminant) / (2.0 * a), (-b - sqrt discriminant) / (2.0 * a)]
        else ([(-b / (2*a)) | discriminant == 0])
        where
            discriminant = b*b - 4*a*c
    Polynomial [a,b,c,d]    -> error "Cubic equation not yet implemented."
    Polynomial [a,b,c,d,e]  -> error "Quartic equation not yet implemented."
    _                       -> error $ "There is no exact formula for the roots of a degree-" ++ show (degree poly) ++ " polynomial."

{- ========================================
    RATIONAL FUNCTIONS
======================================== -}

type RationalFunc = FracType PolynomType

ratioDivMod :: RationalFunc -> (PolynomType, PolynomType)
ratioDivMod r = divMod (numerator r) (denominator r)

instance RealFunction RationalFunc where
  eval :: RationalFunc -> CoeffVar -> CoeffVar
  eval (Fraction fn fd) x = eval fn x / eval fd x
  fromReal :: Real r => r -> RationalFunc
  fromReal c = Fraction (fromReal c) (fromReal 1)
  derivative :: RationalFunc -> RationalFunc
  derivative (Fraction f g) = new_num % new_denom   -- (f/g)' = (f'g - fg')/(g^2)
    where
        new_num     = f' * g - f * g'
        new_denom   = g * g
        f' = derivative f
        g' = derivative g
  shiftH :: RationalFunc -> CoeffVar -> RationalFunc
  shiftH (Fraction fn fd) c = Fraction (shiftH fn c) (shiftH fd c)
  flipH :: RationalFunc -> RationalFunc
  flipH (Fraction fn fd) = Fraction (flipH fn) (flipH fd)
  scaleH :: RationalFunc -> CoeffVar -> RationalFunc
  scaleH (Fraction fn fd) c = Fraction (scaleH fn c) (scaleH fd c)

chebyshevPoly1 :: Integral a => a -> PolynomType
chebyshevPoly1 n
    | n == 0    = 1
    | n == 1    = x
    | otherwise = 2 * x * chebyshevPoly1 n - chebyshevPoly1 (n-1)
    where x = Polynomial [0, 1]

chebyshevPoly2 :: Integral a => a -> PolynomType
chebyshevPoly2 n
    | n == 0    = 1
    | n == 1    = 2 * x
    | otherwise = 2 * x * chebyshevPoly1 n - chebyshevPoly1 (n-1)
    where x = Polynomial [0, 1]

-- coefficient of t^n in the Taylor series expansion of 1/sqrt(1 - 2xt + t^2) around t=0
-- IMPLEMENTATION: Rodrigues' formula (recursive)
legendrePoly :: Integral a => a -> PolynomType
legendrePoly n = multiapply n derivative (x_squared_minus_one^n) `div` fromIntegral ((2^n) * factorial n) where
    x_squared_minus_one = Polynomial [-1,0,1]

-- shifted so that Pshifted(n,x) = P(n,2x-1)
legendrePolyShifted :: Integral a => a -> PolynomType
legendrePolyShifted n = multiapply n derivative (x_squared_minus_x^n) `div` fromIntegral (factorial n) where
    x_squared_minus_x = Polynomial [0,-1,1]

-- Laguerre polynomials: Solutions of differential equation xy'' + (1-x)y' + ny=0
laguerrePoly :: Integral a => a -> PolynomType
laguerrePoly n = sum $ map lagterm [0..n] where
    lagterm :: Integral b => b -> PolynomType
    lagterm k = (fromIntegral (combination n (fromIntegral k)) :: PolynomType) * (negx ^ k) `div` fromIntegral (factorial k) where negx = Polynomial [0,-1]

hermitePoly_prob :: Integral a => a -> PolynomType
hermitePoly_prob n
    | n == 0        = 1
    | n == 1        = x
    | otherwise     = x * hermite_n_minus_1 - derivative hermite_n_minus_1
    where
        x = Polynomial [0,1]
        hermite_n_minus_1 = hermitePoly_prob (n-1)

hermitePoly_phys :: Integral a => a -> PolynomType
hermitePoly_phys n
    | n == 0        = 1
    | n == 1        = 2 * x
    | otherwise     = 2 * x * hermite_n_minus_1 - derivative hermite_n_minus_1
    where
        x = Polynomial [0,1]
        hermite_n_minus_1 = hermitePoly_phys (n-1)

fibonacciPoly :: Integral a => a -> PolynomType
fibonacciPoly n
    | n <  0        = neg1power (n-1) * fibonacciPoly (negate n)
    | n == 0        = 0
    | n == 1        = 1
    | otherwise     = x * fibonacciPoly (n-1) + fibonacciPoly (n-2)
    where
        x = Polynomial [0,1]

lucasPoly :: Integral a => a -> PolynomType
lucasPoly n
    | n <  0        = neg1power n * lucasPoly (negate n)
    | n == 0        = 2
    | n == 1        = x
    | otherwise     = x * lucasPoly (n-1) + lucasPoly (n-2)
    where
        x = Polynomial [0,1]

abelPoly :: Integral a => CoeffVar -> a -> PolynomType
abelPoly a n = x * ((x - an)^(n-1)) where
    x = Polynomial [0,1]
    an = Polynomial [a * fromIntegral n]

fallingFactorialPoly :: Integral a => a -> PolynomType
fallingFactorialPoly n = product $ map falling [0..(n-1)] where
    falling k = Polynomial [fromIntegral (-k) :: Double, 1]

risingFactorialPoly :: Integral a => a -> PolynomType
risingFactorialPoly n = product $ map falling [0..(n-1)] where
    falling k = Polynomial [fromIntegral k :: Double, 1]

allOnePoly :: Integral a => a -> PolynomType
allOnePoly n = Polynomial $ replicate (fromIntegral (n+1)) 1

touchardPoly :: Integral a => a -> PolynomType
touchardPoly n = Polynomial $ map (fromIntegral . stirling2 n') [0..n'] where
    n' = fromIntegral n

batemanPoly :: Integral a => a -> PolynomType
batemanPoly n
    | n == 0    = Polynomial [1]
    | n == 1    = Polynomial [0,-1]
    | otherwise = ((-2*n_1 - 1) * x * batemanPoly (n-1) + n_1*n_1*batemanPoly (n-2))  `div` (n_0 ^ 2)
    where
        x   = Polynomial [0,1]
        n_0 = Polynomial [fromIntegral n]
        n_1 = Polynomial [fromIntegral (n-1)]

{-
bernoulliPoly :: Integral a => a -> PolynomType
bernoulliPoly n
    | n == 0    = Polynomial [1]
    | 
-}
