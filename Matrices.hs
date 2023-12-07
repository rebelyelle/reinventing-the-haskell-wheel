{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Use camelCase" #-}

module Matrices where

import BrentHelpers ( alternateSigns, makeArrayOfIndices, map2D )

-- helpful typedefs
type Element    = Double
type Row        = [Element]

{- ========================================
    TYPE DEFINITION & IMPLEMENTATION
======================================== -}
-- type constructor (constructs Matrix from an array)
newtype Matrix = Matrix [[Element]]
-- inverse type constructor (extracts the underlying array from a matrix)
deMatrix :: Matrix -> [[Element]]
deMatrix (Matrix m) = m

instance Show Matrix where
    show :: Matrix -> String
    show mat@(Matrix m)
        | length m == 1         = "[" ++ showRow (mat `row` 0) ++ "]"
        | otherwise             = "[" ++ showRow (mat `row` 0) ++ "]\n" ++ show (Matrix (tail m))
        where
            showRow r
                | length r == 1     = show (head r)
                | otherwise         = show (head r) ++ "\t" ++ showRow (tail r)
instance Eq Matrix where
    (==) :: Matrix -> Matrix -> Bool
    (==) mat1 mat2 = deMatrix mat1 == deMatrix mat2
instance Num Matrix where
    (+) :: Matrix -> Matrix -> Matrix
    (+) mat1@(Matrix m1) mat2@(Matrix m2) =
        if dims mat1 == dims mat2
        then Matrix $ (zipWith . zipWith) (+) m1 m2
        else error $ "Cannot add matrices of size " ++ show (dims mat1) ++ " and " ++ show (dims mat2)
    (-) :: Matrix -> Matrix -> Matrix
    (-) mat1@(Matrix m1) mat2@(Matrix m2) =
        if dims mat1 == dims mat2
        then Matrix $ (zipWith . zipWith) (-) m1 m2
        else error $ "Cannot subtract matrices of size " ++ show (dims mat1) ++ " and " ++ show (dims mat2)
    (*) :: Matrix -> Matrix -> Matrix
    (*) (Matrix m1) (Matrix m2) =
        if colCount (Matrix m1) == rowCount (Matrix m2)
        then transpose . Matrix . map (multiplyMatrixAndVector m1) . deMatrix . transpose . Matrix $ m2
        else error $ "Cannot multiply matrices of size " ++ show (dims (Matrix m1)) ++ " and " ++ show (dims (Matrix m1))
        where
            multiplyMatrixAndVector matrix vector
                | rowCount (Matrix matrix) == 1      = [dotprod (Matrix matrix `col` 0) vector]
                | otherwise                 = zipWith dotprod matrix (replicate n vector) where
                    n = rowCount (Matrix matrix)
    abs :: Matrix -> Matrix
    abs m = error "Cannot take absolute value of a matrix. Perhaps you meant to use 'determinant'?"
    signum :: Matrix -> Matrix
    signum m = error "Cannot take signum function of a matrix. Perhaps you meant to use 'determinant'?"
    fromInteger :: Integer -> Matrix
    fromInteger i = error "Cannot directly convert an integer to a matrix due to dimensional ambiguity. Use a scaled identity matrix instead."

{- ========================================
    OTHER ARITHMETIC
======================================== -}

-- multiply matrix by a scalar value
scale :: Element -> Matrix -> Matrix
scale scalar = Matrix . map2D (* scalar) . deMatrix

{- ========================================
    MATRIX GENERATION
======================================== -}

-- generate the (n,n) identity matrix
identity :: Int -> Matrix
identity n = Matrix (map2D kroenecker $ makeArrayOfIndices k k) where
    k = n
    kroenecker (x,y)
        | x==y      = 1
        | otherwise = 0

-- example matrices for testing
mata :: Matrix
mata = Matrix [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
matb :: Matrix
matb = Matrix [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
matc :: Matrix
matc = Matrix [[1], [2], [3]]
matd :: Matrix
matd = Matrix [[2, -3, 1], [2, 0, -1], [1, 4, 5]]

{- ========================================
    ELEMENT ACCESS & FUNDAMENTAL FEATURES
======================================== -}
-- accessing elements, rows, and columns
ind :: Matrix -> Int -> Int -> Element
ind (Matrix m) i j = m !! i !! j
indPair :: Matrix -> (Int, Int) -> Element
indPair matrix (i, j) = ind matrix i j
row :: Matrix -> Int -> Row
(Matrix m) `row` i = m !! i
col :: Matrix -> Int -> Row
(Matrix m) `col` j = map (!! j) m
colv :: Matrix -> Int -> Matrix
m `colv` j = transpose $ Matrix [m `col` j]

-- verifiers
isMatrix :: Matrix -> Bool
isMatrix (Matrix m) = all (== firstRowLength) rowlengths where
    rowlengths      = map length m
    firstRowLength  = head rowlengths
isSquare :: Matrix -> Bool
isSquare matrix = isMatrix matrix && rowCount matrix == colCount matrix

-- checking dimensions
rowCount :: Matrix -> Int
rowCount (Matrix m) = length m
colCount :: Matrix -> Int
colCount (Matrix m) = (length . head) m
diagCount :: Matrix -> Int
diagCount m = min (rowCount m) (colCount m)
dims :: Matrix -> (Int, Int)
dims m = (rowCount m, colCount m)

{- ========================================
    CALCULATIONS RESULTING IN SCALARS
======================================== -}
trace :: Matrix -> Element
trace (Matrix [[x]]) = x
trace matrix =
    if isSquare matrix
    then ind matrix 0 0 + trace submat
    else error $ "Cannot calculate trace of non-square matrix. Given dimensions: " ++ show (dims matrix)
    where
        submat = submatrix matrix 0 0

determinant :: Matrix -> Element
determinant (Matrix [[x]])               = x
determinant (Matrix [[a, b], [c, d]])    = a*d - b*c
determinant matrix              =
    if isSquare matrix
    then dotprod altfirst subdets
    else error $ "Cannot calculate determinant of non-square matrix. Given dimensions: " ++ show (dims matrix)
    where
        altfirst = alternateSigns (matrix `row` 0)
        subdets  = map determinant $ submatrices 0 matrix

-- DOT PRODUCTS: Extra functions here so we can avoid doing transposition and full-on matrix multiplication in places.
-- calculate the dot product (Euclidean inner product) of two row-vectors
dotprod :: Row -> Row -> Element
dotprod rv1 rv2 =
    if length rv1 == length rv2
    then sum $ zipWith (*) rv1 rv2
    else error $ "Cannot dot-multiply vectors of length " ++ show (length rv1) ++ " and " ++ show (length rv2) ++ "."
-- calculate the dot product of two column vectors
dotprodv :: Matrix -> Matrix -> Element
dotprodv cv1 cv2 =
    if colCount cv1 == 1 && colCount cv2 == 1
    then dotprod (col cv1 0) (col cv2 0)
    else error $ "Cannot dot-multiply non-column matrices as vectors. Given dimensions: " ++ show (dims cv1) ++ " and " ++ show (dims cv2)

{- ========================================
    CALCULATIONS RESULTING IN NEW MATRICES
======================================== -}
--transpose the current matrix
transpose :: Matrix -> Matrix
transpose ma@(Matrix m)
    | colCount ma == 1          = Matrix [ma `col` 0]
    | otherwise                 = Matrix (ma `col` 0 : rest_of_matrix)
    where
        Matrix rest_of_matrix = transpose (Matrix (map tail m))

-- returns the submatrix given by removing row i and column j
submatrix :: Matrix -> Int -> Int -> Matrix
submatrix matrix i j = transpose . Matrix . remove j . deMatrix . transpose . Matrix . remove i $ deMatrix matrix where
    remove 0 (x:xs) = xs
    remove i (x:xs) = x : remove (i-1) xs
-- returns all submatrices obtainable by eliminating row i and every choice of column
submatrices :: Int -> Matrix -> [Matrix]
submatrices i matrix = map (submatrix matrix i) [0..n] where
    n = colCount matrix - 1

-- returns the cofactor matrix, where each element is +/-1 times the determinant of the submatrix formed by eliminating the element's given row & column
cofactor :: Matrix -> Matrix
cofactor matrix =
    if isSquare matrix                  -- calculation depends on determinant, which requires square matrix
    then Matrix $ map2D (\(i,j) -> cofactorIJ matrix i j) (makeArrayOfIndices rMax cMax)
    else error $ "Cannot calculate cofactors of non-square matrix. Given dimensions: " ++ show (dims matrix)
    where
        rMax                    = rowCount matrix
        cMax                    = colCount matrix
        cofactorIJ matrix i j   = (-1.0)^(i+j) * determinant (submatrix matrix i j)

-- returns the adjugate matrix, the transpose of the cofactor matrix
adjugate :: Matrix -> Matrix
adjugate matrix =
    if isSquare matrix                  -- calculation depends on determinant, which requires square matrix
        then (transpose . cofactor) matrix
        else error $ "Cannot calculate adjugate of non-square matrix. Given dimensions: " ++ show (dims matrix)

-- returns the inverse matrix: if B is the inverse of A, then A*B == B*A == I(n), where I(n) is the n-by-n identity matrix
inverse :: Matrix -> Matrix
inverse matrix =
    if isSquare matrix                  -- invertible matrices must be square
    then if determinant matrix /= 0     -- invertible matrices must have nonzero determinant
         then scale (1.0 / determinant matrix) (adjugate matrix)
         else error "Cannot calculate inverse of zero-determinant matrix."
    else error $ "Cannot calculate inverse of non-square matrix. Given dimensions: " ++ show (dims matrix)

{-      TODO
rank
booleans: isUpperTriangular, isLowerTriangular, isDiagonal, isNull
pseudo-inverse
gaussian/gauss-jordan elimination
matrix decomposition (principal value, LU, QR)
eigenvalues & eigenvectors
characteristic polynomial
-}
