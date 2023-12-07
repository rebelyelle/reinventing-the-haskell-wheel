{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module PeanoNumbers where
import GHC.Real (numerator, denominator)
import SetTheory ( SetOf(..), ensureToSet, union )

{- ========================================
    Peano Natural Numbers
----------------------------------------------------------
Peano natural numbers are a way of constructing the natural numbers from two basic fundamentals:
    - an "atom" that we call Zero
    - a "constructor" Succ (for "successor"), which takes a Peano natural number A and creates a new Peano natural number Succ(A)
In Zermelo-Fraenkel set theory, "Zero" is the empty set {}, and "Succ" consists of taking a set A and creating the set {A, {A}}. We can therefore associate each natural number with a set--essentially, the natural numbers we know and love are repurposed as labels for these sets:
    0   {}
    1   {0,{0}}     = {{},{{}}}
    2   {1,{1}}     = {{{},{{}}},{{{},{{}}}}}
    3   {2,{2}}     = {{{{},{{}}},{{{},{{}}}}},{{{{},{{}}},{{{},{{}}}}}}}
    ...

The reverse of the "Succ" operation would then be retrieving A from {A, {A}}, though this presupposes that there is a set A that can be extracted in this method. Fortunately, this is a *constructed* world, so by design, there are no Peano natural numbers besides {} that can be constructed in this way. Therefore, we define a surjective function "Pred" which maps a Peano natural number {A, {A}} to A, but excludes {} from its domain. We can also refer to Succ as 'incrementing' and Pred as 'decrementing'.

With both Succ and Pred in hand, we can define all arithmetic that stays within the natural numbers:
    - Addition (a+b) is defined by adding 1 to 'a' a total of 'b' times. This is achieved by repeately incrementing 'a', decrementing 'b' each time we do; we continue until b becomes Zero, at which point we stop, and 'a' now holds the sum.
    - Subtraction (a-b) is defined by repeatedly decrementing both 'a' and 'b' until one of them reaches Zero. If 'b' reaches Zero, then 'a' now holds the difference. However, if 'a' reaches Zero and 'b' doesn't, then the result would be a negative number, which is undefined, so an error is thrown.
    - Multiplication (a*b) is defined by repeated addition: We multiply 'a' by itself 'b' times by decrementing 'b' each time we perform a multiplication, stopping once 'b' reaches Zero.
    - Euclidean Division (a/b with remainders) is defined by repeated subtraction of 'b' from 'a', stopping once 'a' becomes less than or equal to 'b'; the number of successful subtractions is the quotient, and the leftover value in 'a' is the remainder.
    - Thanks to having Euclidean division, we can use the Euclidean algorithm to find the GCD of any two Peano naturals, and from there the LCM.

The (Peano) natural numbers form _commutative monoids_ over addition and subtraction; because subtraction and euclidean division being incomplete inverse operations, they are not fully _groups_.
======================================== -}

-- The number 0 is an "atom", and all other natural numbers are defined by "succession" from 0.
data PeanoN = Zero | Succ PeanoN
    deriving (Eq, Ord)

peanoN0 :: PeanoN
peanoN0 = Zero
peanoN1 :: PeanoN
peanoN1 = Succ Zero

-- attempt to strip off one 'Succ' from a Peano number, throwing an error if input is Zero (which has no Succ).
predecessor :: PeanoN -> PeanoN
predecessor (Succ n)   = n
predecessor Zero       = error "Cannot get predecessor of Zero!"

-- 'Maybe'-safe version of the above, returning 'Just _' if there is a predecessor, or 'Nothing' otherwise
predecessor_safe :: PeanoN -> Maybe PeanoN
predecessor_safe (Succ n)   = Just n
predecessor_safe Zero       = Nothing

-- same as 'predecessor', except that Zero is mapped to Zero
predecessor_zero :: PeanoN -> PeanoN
predecessor_zero (Succ n)    = n
predecessor_zero Zero        = Zero

-- regular "show"ing of a Peano natural should show "PeanoN _", where _ is the integer form of the number
instance Show PeanoN where
    show :: PeanoN -> String
    show p = "PeanoN " ++ show (toInteger p)

-- if desired, 'actualN' can show the true form of the Peano natural
actualN :: PeanoN -> String
actualN Zero        = "Zero"
actualN (Succ Zero) = "Succ Zero"
actualN (Succ p)    = "Succ ("++ actualN p ++ ")"

setFormN :: PeanoN -> String
setFormN Zero       = "0"
setFormN (Succ p)   = "{" ++ setFormN p ++ ",{" ++ setFormN p ++ "}" ++ "}"

instance Num PeanoN where
    -- a + b => a + (b-1) + 1 => ... => a + 0 + 1 + ... + 1
    (+) :: PeanoN -> PeanoN -> PeanoN
    p1 + Zero       = p1
    p1 + p2         = Succ (p1 + predecessor p2)
    -- a - b => (a-1) - (b-1) => ... => (a - 1 - ... - 1) - (0)
    (-) :: PeanoN -> PeanoN -> PeanoN
    p1   - Zero     = p1
    Zero - p2       = error "Tried to use peanoSub, but Peano natural numbers cannot go negative!"
    p1   - p2       = predecessor p1 - predecessor p2
    -- a * b => a + a * (b-1) => ... => a + a + ... + a + (a * 0)
    (*) :: PeanoN -> PeanoN -> PeanoN
    _    * Zero     = Zero
    Zero * _        = Zero
    p1   * p2       = p1 + (p1 * predecessor p2)
    -- Peano naturals are always non-negative
    abs :: PeanoN -> PeanoN
    abs p = p
    -- All Peano naturals have sign +1 except Zero, which has sign 0
    signum :: PeanoN -> PeanoN
    signum p
        | p == Zero     = Zero
        | otherwise     = peanoN1
    -- 0 corresponds to Zero, and every other positive number n is built by Succ-ing that n times
    fromInteger :: Integer -> PeanoN
    fromInteger i
        | i < 0         = error "Cannot make Peano natural number from negative number!"
        | i == 0        = Zero
        | otherwise     = Succ $ fromInteger (i-1)
instance Enum PeanoN where
    fromEnum :: PeanoN -> Int
    fromEnum = fromInteger . toInteger
    toEnum :: Int -> PeanoN
    toEnum = fromInteger . toInteger
instance Real PeanoN where
    toRational :: PeanoN -> Rational
    toRational = fromInteger . toInteger
instance Integral PeanoN where
    div :: PeanoN -> PeanoN -> PeanoN
    div _ Zero = error "PeanoN's div/quot: Cannot divide by zero!"
    div p1 p2
        | p2 > p1   = Zero
        | p2 == p1  = peanoN1
        | otherwise = peanoN1 + ((p1 - p2) `div` p2)
    quot :: PeanoN -> PeanoN -> PeanoN
    quot = div
    rem :: PeanoN -> PeanoN -> PeanoN
    rem _ Zero = error "PeanoN's rem/mod: Cannot divide by zero!"
    rem p1 p2
        | p2 > p1  = p1
        | otherwise = (p1 - p2) `mod` p2
    mod :: PeanoN -> PeanoN -> PeanoN
    mod = rem
    toInteger :: PeanoN -> Integer
    toInteger Zero     = 0
    toInteger (Succ n) = 1 + toInteger n
    quotRem :: PeanoN -> PeanoN -> (PeanoN, PeanoN)
    quotRem p1 p2 = (quot p1 p2, rem p1 p2)
    divMod :: PeanoN -> PeanoN -> (PeanoN, PeanoN)
    divMod p1 p2 = (div p1 p2, mod p1 p2)

-- 'Maybe'-safe version of (-) above
safeMinus :: PeanoN -> PeanoN -> Maybe PeanoN
safeMinus p1 p2
    | p2 == Zero    = Just p1
    | p1 == Zero    = Nothing
    | otherwise     = predecessor p1 `safeMinus` predecessor p2

-- OLD DIVISION OPERATORS
{-
-- Euclidean division: returns q, where p1 = q*p2 + r (all Peano naturals, r is as small as possible)
divN :: PeanoN -> PeanoN -> PeanoN
divN _ Zero = error "divN : Cannot divide by zero!"
divN p1 p2
    | p2 > p1   = Zero
    | p2 == p1  = peanoN1
    | otherwise = peanoN1 + ((p1 - p2) `divN` p2)

-- Euclidean division: returns r, where p1 = q*p2 + r (all Peano naturals, r is as small as possible)
modN :: PeanoN -> PeanoN -> PeanoN
modN _ Zero = error "modN : Cannot divide by zero!"
modN p1 p2
    | p2 > p1  = p1
    | otherwise = (p1 - p2) `modN` p2

-- Euclidean algorithm for finding the Greatest Common Divisor
gcdN :: PeanoN -> PeanoN -> PeanoN
gcdN pn1 pn2
    | pn2 == Zero   = Zero
    | pn1 == Zero   = pn2
    | otherwise     = gcdN (a-b) b
    where
        a = max pn1 pn2
        b = min pn1 pn2

-- Probably a more efficient way to do this, but you can't go wrong with the fact that gcd(a,b) * lcm(a,b) = a*b
lcmN :: PeanoN -> PeanoN -> PeanoN
lcmN pn1 pn2 = (pn1 * pn2) `div` gcdN pn1 pn2
-}

{- ========================================
    Peano Integers
----------------------------------------------------------
To expand our universe from natural numbers to all integers, it may be tempting to use the fact that the natural numbers N and the integers Z are sets with the same cardinality, meaning that we can therefore make a simple bijective mapping between them, for example:
    N   0   1   2   3   4   5   6   7   ...
    Z   0   +1  -1  +2  -2  +3  -3  +4  ...
However, using this to define the Peano integers is in fact quite problematic.
Ideally, we would want to be able to "re-use" our algorithms for arithmetic in the Peano natural numbers as much as possible to define integer arithmetic, yet such a mapping from N to Z mapping necessarily jumps back and forth infinitely between the positive and negative numbers, making it difficult to write such algorithms. I am not claiming it is *impossible* to do so--I have not attempted, personally--, but it would be a massive pain. So instead, let us approach the problem from a different direction: how can we conceptualize negative numbers in terms of what we already have from the Peano natural numbers?

The traditional (and likely simplest) way to do this is by conceiving of negative numbers as *differences* of two natural numbers where the second number is larger than the first. In other words, we can conceive of "-5" as "5 - 10". Similarly, we can conceive of positive numbers as differences where the first number is smaller than the second, and zero as differences where the first and second numbers are equal.

How, then, can we achieve this? First, let us take the set of all ordered pairs (a,b) of two Peano natural numbers, and let us mentally "interpret" each ordered pair as a difference: (3,4) represents "3-4", that is, "-1". However, this raises another problem--there are infinite pairs of valid ordered pairs for every desired integer: "-1" is not only (3,4), but also (4,5) and (5,6) and so on. We've actually created "too many" numbers! To cut down on them to just what we want, both of the following seem like straightforward approaches:
    - Limit ourselves to just those ordered pairs with at least one 0 in them. Then, "-1" can only be (0,1), "+1" can only be (1,0), etc.
    - Create an equivalence relation saying that two ordered pairs are "the same" if they represent the same difference.
Here, we take the latter approach, as it seems less... intrusive(?) to use an equivalence relation instead of discarding ordered pairs like (3,4) from validity. (I have not examined any difference in performance there might be between the two methods, if any.) Regardless of the choice we make here, both of them allow us to directly "up-cast" Peano natural numbers to Peano integers via a bijection x → (x,0), placing the Peano natural number x in the first slot and the Peano natural number 0 in the second slot.

For our equivalence relation, we therefore declare:
    - (a1,b1) ~ (a2,b2) in the Peano integers __iff__ a1+b2 == a2+b1 in the Peano natural numbers

One will definitely be tempted to use "a1-b1 == a2-b2 in the Peano natural numbers" here, but don't be fooled: recall that subtraction 'a-b' is *undefined* if 'b' > 'a'! We therefore borrow from pre-algebra the fact that we can rearrange "a1-b1 = a2-b2" into "a1+b2 = a2+b1", using only addition, which *is defined* for all Peano natural numbers. But make no mistake: we are not claiming a priori that we can rearrange Peano natural number equations like this; rather, we can "cheat" by "arbitrarily" imposing the above equivalence relation, with the external foreknowledge that it will in fact define a set that we can easily associate with our everyday integers.

The arithmetic operations for Peano integers can be easily defined by bootstrapping them off the operations already defined for Peano natural numbers:
    - Addition:         (a1,b1) + (a2,b2) = (a1+a2,b1+b2)                       because     (a1-b1)+(a2-b2) = (a1+a2)-(b1+b2)
    - Subtraction:      (a1,b1) - (a2,b2) = (a1+b2,b1+a2)                       because     (a1-b1)-(a2-b2) = (a1+b2)-(a2+b1)
    - Multiplication:   (a1,b1) + (a2,b2) = (a1*a2 + b1*b2, b1*a2 + a1*b2)      because     (a1-b1)+(a2-b2) = (a1*a2+b1*b2)-(b1*a2+a1*b2)
Division and related operations are actually "cheated" by directly using the ones for Peano natural numbers; this is achieved by first converting each ordered pair (a,b) into its "standard form" (c,0) or (0,c), grabbing the non-zero terms, running the natural-number operation on those, then adding a sign term if necessary. Modular division (modZ) in particular is defined in such a way that the same equivalent values for using `modZ` and GHC's built-in `mod` for integers.

The (Peano) integers form a Euclidean domain (a type of ring), but not a field, since Euclidean division is not a complete operation and thus not invertible.
======================================== -}
data PeanoZ = PeanoZ {pos_part :: PeanoN, neg_part :: PeanoN}

peanoZ0 :: PeanoZ
peanoZ0 = PeanoZ Zero Zero
peanoZ1 :: PeanoZ
peanoZ1 = PeanoZ peanoN1 Zero

instance Show PeanoZ where
    show :: PeanoZ -> String
    show pz@(PeanoZ a b) = "PeanoZ " ++ show (toInteger a) ++ " - " ++ show (toInteger b) ++ " =: " ++ show (toInteger pz)

actualZ :: PeanoZ -> String
actualZ pz@(PeanoZ a b) = "PeanoZ " ++ actualN a ++ " " ++ actualN b

instance Eq PeanoZ where
    (==) :: PeanoZ -> PeanoZ -> Bool
    (==) pz1@(PeanoZ a1 b1) pz2@(PeanoZ a2 b2)  = (a1 + b2) == (a2 + b1)
instance Ord PeanoZ where
    (<=) :: PeanoZ -> PeanoZ -> Bool
    (<=) pz1@(PeanoZ a1 b1) pz2@(PeanoZ a2 b2)  = (a1 + b2) <= (a2 + b1)
instance Num PeanoZ where
    (+) :: PeanoZ -> PeanoZ -> PeanoZ
    (PeanoZ a1 b1) + (PeanoZ a2 b2)             = PeanoZ (a1+a2) (b1+b2)
    (*) :: PeanoZ -> PeanoZ -> PeanoZ
    (*) pz1@(PeanoZ a1 b1) pz2@(PeanoZ a2 b2)   = PeanoZ (a1*a2 + b1*b2) (b1*a2 + a1*b2)
    abs :: PeanoZ -> PeanoZ
    abs p = case p' of
        PeanoZ Zero pn      -> PeanoZ pn Zero
        _                   -> p'
        where p' = standardZ p
    signum :: PeanoZ -> PeanoZ
    signum p = case p' of
        PeanoZ Zero Zero    -> PeanoZ Zero     Zero
        PeanoZ pn Zero      -> PeanoZ peanoN1 Zero
        _                   -> PeanoZ Zero     peanoN1
        where p' = standardZ p
    negate :: PeanoZ -> PeanoZ
    negate (PeanoZ a b) = PeanoZ b a
    fromInteger :: Integer -> PeanoZ
    fromInteger i
        | i > 0     = PeanoZ (fromInteger i) Zero
        | otherwise = PeanoZ Zero (fromInteger i)
instance Enum PeanoZ where
    fromEnum :: PeanoZ -> Int
    fromEnum = fromInteger . toInteger
    toEnum :: Int -> PeanoZ
    toEnum = fromInteger . toInteger
instance Real PeanoZ where
  toRational :: PeanoZ -> Rational
  toRational = fromInteger . toInteger
instance Integral PeanoZ where
    quotRem :: PeanoZ -> PeanoZ -> (PeanoZ, PeanoZ)
    quotRem n1@(PeanoZ a1 b1) n2@(PeanoZ a2 b2) = (quotient n1 n2, remainder n1 n2)
        where
            quotient pz1 pz2 = signum pz1 * signum pz2 * PeanoZ q Zero
                where q = div (pos_part $ abs pz1) (pos_part $ abs pz2)
            remainder pz1 pz2
                | signum pz1 == peanoZ1 && signum pz2 /= peanoZ1    =               pz2 + PeanoZ r Zero
                | signum pz1 /= peanoZ1 && signum pz2 /= peanoZ1    = signum pz2 *        PeanoZ r Zero
                | signum pz1 /= peanoZ1 && signum pz2 == peanoZ1    =               pz2 - PeanoZ r Zero
                | otherwise                                         =                     PeanoZ r Zero
                where
                    q = div (pos_part $ abs pz1) (pos_part $ abs pz2)
                    r = mod (pos_part $ abs pz1) (pos_part $ abs pz2)
    toInteger :: PeanoZ -> Integer
    toInteger p = case p' of
        PeanoZ pn Zero  ->          toInteger $ pos_part p'
        _               -> negate $ toInteger $ neg_part p'
        where p' = standardZ p

standardZ :: PeanoZ -> PeanoZ
standardZ p@(PeanoZ _ Zero) = p
standardZ p@(PeanoZ Zero _) = p
standardZ p@(PeanoZ n1 n2)  = standardZ $ PeanoZ (predecessor n1) (predecessor n2)

{-
-- OLD DIVISION OPERATORS
gcdZ :: PeanoZ -> PeanoZ -> PeanoZ
gcdZ pz1 pz2 = signum pz1 * signum pz2 * PeanoZ absgcd Zero where
    absgcd = gcdN (pos_part $ abs pz1) (pos_part $ abs pz2)

divZ :: PeanoZ -> PeanoZ -> PeanoZ
divZ pz1 pz2 = signum pz1 * signum pz2 * PeanoZ quotient Zero where
    quotient = div (pos_part $ abs pz1) (pos_part $ abs pz2)

modZ :: PeanoZ -> PeanoZ -> PeanoZ
modZ pz1 pz2
    | signum pz1 == peanoZ1 && signum pz2 /= peanoZ1    =               pz2 + PeanoZ remainder Zero
    | signum pz1 /= peanoZ1 && signum pz2 /= peanoZ1    = signum pz2 *        PeanoZ remainder Zero
    | signum pz1 /= peanoZ1 && signum pz2 == peanoZ1    =               pz2 - PeanoZ remainder Zero
    | otherwise                                         =                     PeanoZ remainder Zero
    where
        remainder = mod (pos_part $ abs pz1) (pos_part $ abs pz2)
-}

{- ========================================
    Peano Rationals
----------------------------------------------------------
The vast majority of the logic for why we construct the Peano rationals in the way that we do is identical or equivalent for why we construct the Peano integers in the above fashion, so I will try to avoid repeating myself needlessly. A rational number can be defined by an ordered pair of integers--the numerator and denominator--but with an equivalence relation to take care of the fact that fractions can be in non-reduced form, i.e., 2/3 and 4/6 represent the same number. This time, our equivalence relation is:
    - (n1,d1) ~ (n2,d2) in the Peano rationals __iff__ n1*d2 == n2*d1 in the Peano integers
Here, we use cross-multiplication (hooray for pre-algebra) rather than "n1/d1 == n2/d2" because (Euclidean) division is not a complete operation in the Peano integers, whereas multiplication is.

This construction lets us up-cast Peano integers into the Peano rationals by the mapping x → (x,1), placing the Peano integer x in the numerator and the Peano integer +1 in the denominator.

Arithmetic operations:
    - Addition & Subtraction: Implemented here by first re-scaling both fractions "n1/d1" and "n2/d2" to have the same common denominator "d1*d2", adding or subtracting, then reducing. The product is used rather than the LCM to try and minimize invocations of the Euclidean algorithm to find the GCDs.
    - Multiplcation & Division: Extremely easy, just as in traditional arithmetic.

The (Peano) rationals form a field--both multiplication and division are perfectly invertible (minus the allowed exception for 0 having no multiplicative inverse).
======================================== -}

data PeanoQ = PeanoQ {num_part :: PeanoZ, den_part :: PeanoZ}

peanoQ0 :: PeanoQ
peanoQ0 = PeanoQ peanoZ0 peanoZ1
peanoQ1 :: PeanoQ
peanoQ1 = PeanoQ peanoZ1 peanoZ1

instance Show PeanoQ where
    show :: PeanoQ -> String
    show (PeanoQ pz1 pz2) = "PeanoQ "++ show (toInteger pz1) ++ " ÷ " ++ show (toInteger pz2) ++ " =: " ++ show (toInteger pz1') ++ "/" ++ show (toInteger pz2') where (PeanoQ pz1' pz2') = standardQ (PeanoQ pz1 pz2)

actualQ :: PeanoQ -> String
actualQ pq@(PeanoQ n d) = "PeanoQ " ++ actualZ n ++ " " ++ actualZ d

instance Eq PeanoQ where
    (==) :: PeanoQ -> PeanoQ -> Bool
    (==) pq1@(PeanoQ pza1 pzb1) pq2@(PeanoQ pza2 pzb2) = pza1 * pzb2 == pza2 * pzb1
instance Num PeanoQ where
    (+) :: PeanoQ -> PeanoQ -> PeanoQ
    (+) pq1@(PeanoQ pza1 pzb1) pq2@(PeanoQ pza2 pzb2) = PeanoQ newNum newDen where
        newNum = pza1 * pzb2 + pza2 * pzb1
        newDen = pzb1 * pzb2
    (-) :: PeanoQ -> PeanoQ -> PeanoQ
    (-) pq1@(PeanoQ pza1 pzb1) pq2@(PeanoQ pza2 pzb2) = PeanoQ newNum newDen where
        newNum = pza1 * pzb2 - pza2 * pzb1
        newDen = pzb1 * pzb2
    (*) :: PeanoQ -> PeanoQ -> PeanoQ
    (*) pq1@(PeanoQ pza1 pzb1) pq2@(PeanoQ pza2 pzb2) = PeanoQ (pza1*pza2) (pzb1*pzb2)
    abs :: PeanoQ -> PeanoQ
    abs pq@(PeanoQ pza pzb) = PeanoQ (abs pza) (abs pzb)
    signum :: PeanoQ -> PeanoQ
    signum pq@(PeanoQ pza pzb)
        | pza == peanoZ0    = PeanoQ peanoZ0 peanoZ1
        | abs pq == pq      = PeanoQ peanoZ1 peanoZ1
        | otherwise         = PeanoQ (negate peanoZ1) peanoZ1
    fromInteger :: Integer -> PeanoQ
    fromInteger i = PeanoQ (fromInteger i) peanoZ1
    negate :: PeanoQ -> PeanoQ
    negate pq@(PeanoQ pza pzb) = PeanoQ (negate pza) pzb
instance Fractional PeanoQ where
    fromRational :: Rational -> PeanoQ
    fromRational rat = PeanoQ (fromInteger n) (fromInteger d) where
        n = numerator rat
        d = denominator rat
    recip :: PeanoQ -> PeanoQ
    recip pq@(PeanoQ n d) = PeanoQ d n

standardQ :: PeanoQ -> PeanoQ
standardQ pq@(PeanoQ pza pzb)
    | pzb == peanoZ0    = error "Tried to reduce a PeanoQ with a denominator of 0!"
    | otherwise         = PeanoQ newA newB
    where
        newA = signum pza * signum pzb * abs pza `div` reducer
        newB = abs $ pzb `div` reducer
        reducer = abs $ gcd pza pzb