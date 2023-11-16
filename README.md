# reinventing-the-haskell-wheel
Custom-built Haskell modules for various math-y purposes.

## Fractions.hs
Provides functionality for a `Fraction` type, which supports arithmetic and exponents with fractions.
* **arithmetic:** addition, subtraction, multiplication, division
* **equivalency:** change to most-reduced form, scaling-up numerator & denominator
* **conversion:** conversion to floating-point, conversion to 

### to be fixed
* **equivalency:** fix reducing function to also take care of negative signs

### to be added
* **booleans:** comparison operators

## Matrices.hs
Provides functionality for a `Matrix` type (really a `type` of ``[[Double]]``):
* row & column access
* trace, determinant
* transpose, cofactor, adjugate, inverse, submatrices
* arithmetic: addition, subtraction, scalar multiplication, matrix multiplication
* dot product for row-vectors and column-vectors

## NumberTheory.hs
Provides functions relating to number theory, including:
* set cardinality and powerset (borrowing from Set Theory)
* Kronecker delta, 2D and 3D Levi-Civita symbols
* factorial, permutation, combination
* **divisibility:** determining divisors & common factors, factor multiplicity, common multiples, coprimality, totatives & Euler's totient function, aliquot sum, abundance/deficiency, determining square-free-ness, Fermat's primality test, Bézout coefficients from Extended Euclidean Algorithm
* **prime numbers:** all primes, primality test, mersenne primes, prime factorization, Ω(n) and ω(n) functions, prime-counting function π(n), Liouville's function λ(n), Möbius function μ(n), Mertens function M(n), primorial function n#, Chebyshev functions ϑ(n) and ψ(n), Von Mangoldt function Λ(n)
* **prime-like qualities:** *k*-almost primes, Carmichael and Lucas-Carmichael numbers, Strømer numbers, *k*-pseudoprimes, perfect numbers, semiperfect numbers, abundant & deficient numbers, weird numbers, *k*-smooth and *k*-rough numbers, practical numbers
* **integer sequences:** Lucas sequences of the first kind (Fibonacci, Pell, Jacobsthal), Lucas sequences of the 2nd kind (Lucas, Pell-Lucas, Jacobsthal-Lucas), Sylvester's sequence, Göbel's sequence, Catalan numbers *C<sub>n</sub>*, Bell numbers *B<sub>n</sub>*, Stirling numbers of the 1st and 2nd kinds, Bernoulli numbers *B<sup>+</sup><sub>n</sub>*
* **miscellaneous:** reversing the digits in an integer or floating-point number

## Polynomials.hs
Provides functionality for a `Polynomial` type (really a `type` of `[Double]`). A polynomial of the form *p(x) = a<sub>0</sub> + a<sub>1</sub>x + a<sub>2</sub>x<sup>2</sup> + ... + a<sub>n</sub>x<sup>n</sup>* is stored as a list of coefficients `[a0, a1, ..., an]`.
* **arithmetic:** addition, subtraction, multiplication, evaluation of *p(x)*
* **features:** degree
* **calculus:** differentiation *p(x) → dp/dx*, antidifferentiation *p(x) → ∫ p(x) dx*, integral *p(x) → ∫<sub>a</sub><sup>b</sup> p(x) dx*

### to be fixed
* **arithmetic:** addition & subtraction currently bugged, dropping higher-order coefficients

### to be added
* **features:** zeroes, extrema (zeroes of *dp/dx*), turning points (zeroes of *d<sup>2</sup>p/dx<sup>2</sup>*)
* **arithmetic:** Euclidean division of polynomials (quotient & remainder)
