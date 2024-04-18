# reinventing-the-haskell-wheel
Custom-built Haskell modules for various math-y purposes.

## BitIntegers.hs
Implements binary integers as sequences of bits. First, a `Bit` type is created, with corresponding unary, binary, and ternary (adder) operations. Next, a `BitInt` type is defined as a list of `Bit`s, with addition implemented.

### to be changed
Change `Bit` and `BitInt` types to their proper typeclasses to overload operators, perhaps creating a new typeclass?

## BrentHelpers.hs
A collection of broadly-useful functions that I am likely to want to import all over different places.

## Fractions.hs
Provides functionality for a `FracType` type constructor, which takes a type `a` and creates the `FracType a` type, which consists of `Fraction`s with a numerator and denominator of type `a`. Depending on the type `a`, a greater or lesser number of additional functionality can be available:
* **conversion:** % operator can be used to make any two same-type values `x` and `y` into `Fraction x y`
* **arithmetic:** addition, subtraction, multiplication, division, absolute value, sign, finding LCD
* **comparison:** equality and LE/LEQ comparison operators
* **equivalency:** change to most-reduced form, scaling-up numerator & denominator
* **conversion:** conversion to floating-point, conversion from floating-point, rounding fraction to new denominator

## IntegerSequences.hs
Provides functions for calculating all sorts of [integer sequences](https://en.wikipedia.org/wiki/Integer_sequence). Originally part of `NumberTheory.hs`, but branched off once it became too cumbersome to have in the same file.

## Matrices.hs
Provides functionality for a `Matrix` type (a wrapped `[[Double]]`):
* **features:** row & column & element access, trace, determinant
* **arithmetic:** addition, subtraction, scalar multiplication, matrix multiplication, dot product for row-vectors and column-vectors
* **related matrices:** transpose, cofactor, adjugate, inverse, submatrices

### to be added
* **boolean tests:** isUpperTriangular, isLowerTriangular, isDiagonal, isNull
* gaussian/gauss-jordan elimination --> calculating rank
* matrix decomposition (principal value, LU, QR)
* eigenvalues, eigenvectors, characteristic polynomial (invoking Polynomials.hs?)
* pseudoinverse

## NumberTheory.hs
Provides functions relating to number theory, including:
* set cardinality and powerset (borrowing from Set Theory)
* Kronecker delta, 2D and 3D Levi-Civita symbols
* factorial, permutation, combination
* **divisibility:** determining divisors & common factors, factor multiplicity, common multiples, coprimality, totatives & Euler's totient function, aliquot sum, abundance/deficiency, determining square-free-ness, Fermat's primality test, Bézout coefficients from Extended Euclidean Algorithm
* **prime numbers:** all primes, primality test, mersenne primes, prime factorization, $\Omega(n)$ and $\omega(n)$ functions, prime-counting function $\pi(n)$, Liouville's function $\lambda(n)$, Möbius function $\mu(n)$, Mertens function $M(n)$, primorial function $n$#, Chebyshev functions $\vartheta(n)$ and $\psi(n)$, Von Mangoldt function $\Lambda(n)$
* **prime-like qualities:** $k$-almost primes, Carmichael and Lucas-Carmichael numbers, Strømer numbers, $k$-pseudoprimes, perfect numbers, semiperfect numbers, abundant & deficient numbers, weird numbers, $k$-smooth and $k$-rough numbers, practical numbers
* **integer sequences:** Lucas sequences of the first kind (Fibonacci, Pell, Jacobsthal), Lucas sequences of the 2nd kind (Lucas, Pell-Lucas, Jacobsthal-Lucas), Sylvester's sequence, Göbel's sequence, Catalan numbers $C_n$, Bell numbers $B_n$, Stirling numbers of the 1st and 2nd kinds, Bernoulli numbers $B^+_n$
* **miscellaneous:** reversing the digits in an integer or floating-point number

## PeanoNumbers.hs
Implements a construction of the natural numbers from [Peano axioms](https://en.wikipedia.org/wiki/Peano_axioms), followed by integers using an equivalence class of ordered pairs, followed by rationals by the same. At each level ($\mathbb{N}$, $\mathbb{Z}$, and $\mathbb{Q}$), various functionalities are implemented:

* **arithmetic:** addition, multiplication
  * $\mathbb{N}$: incomplete subtraction (due to lack of negative numbers)
  * $\mathbb{Z}$ and $\mathbb{Q}$: complete subtraction
  * $\mathbb{N}$ and $\mathbb{Z}$: Euclidean division, GCD, LCM
  * $\mathbb{Q}$: complete division
* **ordering:**
  * $\mathbb{N}$: `Succ`essor and `predecessor` operators
  * boolean comparators: `<` `<=` `>` `>=` `==` `/=`
* **printability:** each number can be printed in brief, like `PeanoN 3`, or as its true underlying form, like `Succ (Succ (Succ Zero))`.
* **conversion:** type conversions to and from `Integral` types

## Polynomials.hs
Provides functionality for a `PolynomType` and a `RationalFunc` type (the latter an alias of `FracType PolynomType`), both instances of a `RealFunction` typeclass. A polynomial of the form $p(x) = a_0 + a_1x + a_2x^2 + ... + a_nx^n$ is stored as a wrapped list of coefficients `[a0, a1, ..., an]`.
* **arithmetic:** addition, subtraction, multiplication, evaluation of $p(x)$, Euclidean division
* **transformation:** horizontal & vertical shifting/scaling/reflection, determining if a function is even or odd
* **features:** degree, zeroes (calculated by [Newton's method](https://en.wikipedia.org/wiki/Newton%27s_method))
* **calculus:** limits (one- and two-sided), differentiation $p(x) \rightarrow dp/dx$, forward/backward/central difference
  * for `PolynomType`s: antidifferentiation $p(x) \rightarrow \int p(x) dx$, integration $p(x) \rightarrow \int_a^b p(x) dx$
  * for all `RealFunction`s: numerical integration (trapezoid method)
* **sequences:** provides functions for calculating several different [polynomial sequences](https://en.wikipedia.org/wiki/Polynomial_sequence)
  * Chebyshev, Legendre, Shifted Legendre, Lagurerre, Physicist's Hermite, Probabilist's Hermite, Fibonacci, Lucas, Abel, Falling-Factorial & Rising-Factorial, All-One, Touchard, Bateman

### to be added
* **features:** extrema (zeroes of $dp/dx$), turning points (zeroes of $d^2x/dx^2$)
* **sequences:** there are plenty more to be added

## SetTheory.hs
Creates a type constructor `SetOf a`, which can be `EmptySet` or `Set [a]`. Functionality is are provided for:
* **conversion:**
  * a list `[a]` into a `SetOf a`
  * a list of lists `[[a]]` into a `SetOf (SetOf a)`
  * two elements `x` and `y` into an ordered pair by [Kuratowski's definition](https://en.wikipedia.org/wiki/Ordered_pair#Kuratowski's_definition)
* **operations on elements:** adding an element, checking for an element's presence, removing an element if present, map function onto set, filter set by boolean function
* **unary operations on sets:** finding cardinality, checking if a set is empty or not, checking if a set is properly formed or not, find powerset
* **binary & *n*-ary operations on sets:** union & big union, intersection & big intersection, set difference, cartesian product
* **relation operations:**
  *  equivalence classes: given a set and an [equivalence relation](https://en.wikipedia.org/wiki/Equivalence_relation), construct the set of all equivalence classes (themselves sets) that [partition](https://en.wikipedia.org/wiki/Partition_of_a_set) the original set
  *  [binary](https://en.wikipedia.org/wiki/Binary_relation) and [ternary](https://en.wikipedia.org/wiki/Ternary_relation) relation conversions:
     *  `SetOf (a,a)` ↔ function `a -> a -> Bool`
     *  `SetOf (a,b)` ↔ function `a -> b`
     *  `SetOf (a,b,c)` ↔ function `a -> b -> c`
