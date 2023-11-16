{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use uncurry" #-}
{-# HLINT ignore "Use camelCase" #-}

module Matrices where

type Element    = Double
type Row        = [Element]
type Matrix     = [Row]

alternateRowSigns :: Row -> Row
alternateRowSigns row = zipWith (*) row (cycle [1, -1])

makeMatrixOfIndices :: Int -> Int -> [[(Int, Int)]]
makeMatrixOfIndices rMax cMax = [[(i, j) | j <- [0..cMax-1] ] | i <- [0..rMax-1]]

show_r :: Row -> String
show_r [oneEntry] = show oneEntry
show_r r = show (head r) ++ "\t" ++ show_r (tail r)

show_m :: Matrix -> String
show_m [oneRow] = "[" ++ show_r oneRow ++ "]"
show_m m = "[" ++ show_r (m `row` 0) ++ "]\n" ++ show_m (tail m)

-- We will be using this largely for its ability to act on makeMatrixOfIndices matrices 
--      a = (Int, Int)
--      b = Element, so [[b]] = Matrix
map2D :: (a -> b) -> [[a]] -> [[b]]
map2D = map . map

-- generate the (n,n) identity matrix
identity :: Int -> Matrix
identity n = map2D kroenecker $ makeMatrixOfIndices k k where
    k = n
    kroenecker (x,y)
        | x==y      = 1
        | otherwise = 0

-- example matrices for testing
mata :: Matrix
mata = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
matb :: Matrix
matb = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
matc :: Matrix
matc = [[1], [2], [3]]
matd :: Matrix
matd = [[2, -3, 1], [2, 0, -1], [1, 4, 5]]

-- verifiers
isMatrix :: Matrix -> Bool
isMatrix m = all (== firstRowLength) rowlengths where
    rowlengths      = map length m
    firstRowLength  = head rowlengths

isSquare :: Matrix -> Bool
isSquare matrix = isMatrix matrix && rowCount matrix == colCount matrix

-- checking dimensions
rowCount :: Matrix -> Int
rowCount = length
colCount :: Matrix -> Int
colCount = length . head
diagCount :: Matrix -> Int
diagCount m = min (rowCount m) (colCount m)

dims :: Matrix -> (Int, Int)
dims m = (rowCount m, colCount m)

-- accessing elements, rows, and columns
ind :: Matrix -> Int -> Int -> Element
ind matrix i j = matrix !! i !! j
indPair :: Matrix -> (Int, Int) -> Element
indPair matrix (i, j) = ind matrix i j
row :: Matrix -> Int -> Row
matrix `row` i = matrix !! i
col :: Matrix -> Int -> Row
matrix `col` j = map (!! j) matrix
colv :: Matrix -> Int -> Matrix
matrix `colv` j = transpose [matrix `col` j]

--transpose the current matrix
transpose :: Matrix -> Matrix
transpose matrix
    | colCount matrix == 1      = [matrix `col` 0]
    | otherwise                 = matrix `col` 0 : transpose (map tail matrix)

-- returns the submatrix given by removing row i and column j
submatrix :: Matrix -> Int -> Int -> Matrix
submatrix matrix i j = transpose . remove j . transpose . remove i $ matrix where
    remove 0 (x:xs) = xs
    remove i (x:xs) = x : remove (i-1) xs
-- returns all submatrices obtainable by eliminating row i and every choice of column
submatrices :: Int -> Matrix -> [Matrix]
submatrices i matrix = map (submatrix matrix i) [0..n] where
    n = colCount matrix - 1

trace :: Matrix -> Element
trace [[x]] = x
trace matrix =
    if isSquare matrix
    then ind matrix 0 0 + trace submat
    else error $ "Cannot calculate trace of non-square matrix. Given dimensions: " ++ show (dims matrix)
    where
        submat = submatrix matrix 0 0

determinant :: Matrix -> Element
determinant [[x]]               = x
determinant [[a, b], [c, d]]    = a*d - b*c
determinant matrix              =
    if isSquare matrix
    then dotprod altfirst subdets
    else error $ "Cannot calculate determinant of non-square matrix. Given dimensions: " ++ show (dims matrix)
    where
        altfirst = alternateRowSigns (matrix `row` 0)
        subdets  = map determinant $ submatrices 0 matrix

cofactor :: Matrix -> Matrix
cofactor matrix =
    if isSquare matrix
    then map2D (\(i,j) -> cofactorIJ matrix i j) (makeMatrixOfIndices rMax cMax)
    else error $ "Cannot calculate cofactors of non-square matrix. Given dimensions: " ++ show (dims matrix)
    where
        rMax                    = rowCount matrix
        cMax                    = colCount matrix
        cofactorIJ matrix i j   = (-1.0)^(i+j) * determinant (submatrix matrix i j)

adjugate :: Matrix -> Matrix
adjugate matrix =
    if isSquare matrix
        then (transpose . cofactor) matrix
        else error $ "Cannot calculate adjugate of non-square matrix. Given dimensions: " ++ show (dims matrix)

inverse :: Matrix -> Matrix
inverse matrix =
    if isSquare matrix
    then if determinant matrix /= 0
         then scale (1.0 / determinant matrix) (adjugate matrix)
         else error "Cannot calculate inverse of zero-determinant matrix."
    else error $ "Cannot calculate inverse of non-square matrix. Given dimensions: " ++ show (dims matrix)

add :: Matrix -> Matrix -> Matrix
add m1 m2 =
    if dims m1 == dims m2
    then (zipWith . zipWith) (+) m1 m2
    else error $ "Cannot add matrices of size " ++ show (dims m1) ++ " and " ++ show (dims m2)

sub :: Matrix -> Matrix -> Matrix
sub m1 m2 =
    if dims m1 == dims m2
    then (zipWith . zipWith) (-) m1 m2
    else error $ "Cannot subtract matrices of size " ++ show (dims m1) ++ " and " ++ show (dims m2)

dotprod :: Row -> Row -> Element
dotprod rv1 rv2 =
    if length rv1 == length rv2
    then sum $ zipWith (*) rv1 rv2
    else error $ "Cannot dot-multiply vectors of length " ++ show (length rv1) ++ " and " ++ show (length rv2)

dotprodv :: Matrix -> Matrix -> Element
dotprodv cv1 cv2 =
    if colCount cv1 == 1 && colCount cv2 == 1
    then dotprod (col cv1 0) (col cv2 0)
    else error $ "Cannot dot-multiply non-column matrices as vectors. Given dimensions: " ++ show (dims cv1) ++ " and " ++ show (dims cv2)

mul :: Matrix -> Matrix -> Matrix
mul m1 m2 =
    if colCount m1 == rowCount m2
    then transpose $ map (multiplyMatrixAndVector m1) (transpose m2)
    else error $ "Cannot multiply matrices of size " ++ show (dims m1) ++ " and " ++ show (dims m2)
    where
        multiplyMatrixAndVector matrix vector
            | rowCount matrix == 1      = [dotprod (matrix `col` 0) vector]
            | otherwise                 = zipWith dotprod matrix (replicate n vector) where
                n = rowCount matrix

scale :: Element -> Matrix -> Matrix
scale scalar = map2D (* scalar)