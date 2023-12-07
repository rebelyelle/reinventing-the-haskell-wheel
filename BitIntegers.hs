import BrentHelpers (count)

{- ========================================
    BIT DEFINITION & BINARY OPERATIONS
======================================== -}

maxbits :: Int
maxbits = 32

data Bit = Zero | One
    deriving (Eq, Ord, Show)

bitNOT :: Bit -> Bit
bitNOT b = if b==Zero then One else Zero

bitAND :: Bit -> Bit -> Bit
bitAND = min

bitNAND :: Bit -> Bit -> Bit
bitNAND b1 b2 = bitNOT $ bitAND b1 b2

bitOR :: Bit -> Bit -> Bit
bitOR = max

bitNOR :: Bit -> Bit -> Bit
bitNOR b1 b2 = bitNOT $ bitOR b1 b2

bitXOR :: Bit -> Bit -> Bit
bitXOR b1 b2 = if b1 == b2 then Zero else One

bitXNOR :: Bit -> Bit -> Bit
bitXNOR b1 b2 = if b1 == b2 then One else Zero

bit2int :: Integral a => Bit -> a
bit2int b = if b==Zero then 0 else 1

int2bit :: Integral a => a -> Bit
int2bit i = if i==0 then Zero else One

bitFullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
bitFullAdder a b carryIn = (sum, carryOut) where
    sum         = carryIn `bitXOR` aXORb
    carryOut    = (carryIn `bitAND` aXORb) `bitOR` (a `bitAND` b)
    aXORb       = a `bitXOR` b

{- ========================================
    BIT COUNTERS
======================================== -}

newtype BitInt = BitInt {bitlist :: [Bit]}

trimLeadingZeroes :: BitInt -> BitInt
trimLeadingZeroes (BitInt l)
    | last l == Zero    = trimLeadingZeroes (BitInt $ init l)
    | otherwise         = BitInt l

extendLeadingZeroes :: BitInt -> Int -> BitInt
extendLeadingZeroes bint@(BitInt bi) n
    | length bi >= n     = bint
    | otherwise          = BitInt $ bi ++ neededZeroes
    where
        neededZeroes     = replicate (n - length bi) Zero

fixLength :: BitInt -> BitInt
fixLength bint@(BitInt bi) = BitInt $ take maxbits $ bitlist $ extendLeadingZeroes bint maxbits

instance Show BitInt where
    show :: BitInt -> String
    show (BitInt l)
        | null l            = ""
        | otherwise         =  show (BitInt $ tail l) ++ showBit (head l)
        where
            showBit b       = if head l == One then "1" else "0"
instance Eq BitInt where
    (==) :: BitInt -> BitInt -> Bool
    (==) bi1 bi2 = l1 == l2 where
        BitInt l1 = trimLeadingZeroes bi1
        BitInt l2 = trimLeadingZeroes bi2

byteadd :: BitInt -> BitInt -> BitInt
byteadd bint1@(BitInt b1) bint2@(BitInt b2) = trimLeadingZeroes $ BitInt $ byteadd' (bitlist $ fixLength bint1) (bitlist $ fixLength bint2) Zero

byteadd' :: [Bit] -> [Bit] -> Bit -> [Bit]
byteadd' blist1 blist2 carryIn
    | length blist1 == 1    = [sum, carryOut]
    | otherwise             = sum : byteadd' (tail blist1) (tail blist2) carryOut where
    (sum, carryOut) = bitFullAdder (head blist1) (head blist2) carryIn

byte2int :: BitInt -> Integer
byte2int (BitInt []) = 0
byte2int (BitInt b) = bit2int (head b) + 2 * byte2int (BitInt $ tail b)