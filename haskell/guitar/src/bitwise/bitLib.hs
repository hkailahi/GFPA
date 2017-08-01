module BitLib where

import Data.List
import Text.Printf
import Data.Bits
import Control.Monad
import Control.Applicative

-- 12 bit rotation
rotateL12 :: Int -> Int -> Int
rotateL12 n d = let bitsize = 12
                    bitMask = fromBinDigits $ take bitsize [1,1..]
                    -- xxx00000 get first (12 - d) bits
                    front   = shiftL (shiftR bitMask (d) .&. n) d
                    -- 000xxxxx get rest (d) bits
                    back    = shiftR n (bitsize - d)
                in front .|. back

rotateR12 :: Int -> Int -> Int
rotateR12 n d = let bitsize = 12
                    bitMask = fromBinDigits $ take bitsize [1,1..]
                    -- xxx00000 get first d bits
                    front   = shiftL ((shiftR bitMask (bitsize - d)) .&. n) (bitsize - d)
                    -- 000xxxxx get rest (12 - d) bits
                    back    = (shiftR n d)
                in front .|. back

countSetBits :: Int -> Int -- Kernighan's Algorithm O(log n)
countSetBits 0 = 0
countSetBits n = let x = n .&. (n-1)
                 in 1 + countSetBits(x)

toDigits :: Integral x => x -> [x]
toDigits 0 = []
toDigits x = toDigits (x `div` 10) ++ [x `mod` 10]

toBinDigits :: Int -> [Int]
toBinDigits x = toDigits (read (toBin x) :: Int)

stringDigit :: Int -> String
stringDigit i = foldl (++) "" $ fmap (show) (toDigits i)

stringBinDigit :: Int -> String
stringBinDigit i = foldl (++) "" $ fmap (show) (toBinDigits i)

digitsToString :: [Int] -> String
digitsToString d = foldl (++) "" $ fmap (show) d

binLength :: Int -> Int
binLength 0 = 0
binLength i = length $ toDigits (read (toBin i) :: Int)

matchZeroes :: Int -> Int -> [Int]
matchZeroes a b
  -- returns b with 0s prepended until equal or greater length than a
  | a == 0 && b == 0 = [0]
  | b >= a           = toBinDigits b
  | otherwise        = let zeroes = binLength a - binLength b
                       in replicate zeroes 0 ++ (if b == 0 then [] else toBinDigits b)

fromDigits :: [Int] -> Int
fromDigits []     = 0
fromDigits (x:[]) = x
fromDigits (x:xs) = (foldl (*) 1 $ x : (replicate (length xs) 10)) + fromDigits(xs)

fromBinDigits :: [Int] -> Int
fromBinDigits []     = 0
fromBinDigits (x:[]) = x
fromBinDigits (x:xs) = ( foldl (*) 1 $ x : (replicate (length xs) 2) ) + fromBinDigits(xs)

toBin :: (Integral a, Show a) => a -> [Char]
toBin 0 = []
toBin x =  (toBin $ x `div` 2) ++ (show $ x `mod` 2)

-- ************************** PRINTING HELPER METHODS ************************** --

printBinOp' :: Int -> Int -> Int -> [String]
printBinOp' a b result = let leadingZero x y = digitsToString $ matchZeroes x y
                             a' = leadingZero b a
                             b' = leadingZero a b
                             r'
                                | a > b     = leadingZero a result
                                | otherwise = leadingZero b result
                         in blend (fmap (show) [a,b,result]) [a', b', r']

blend :: [a] -> [a] -> [a]
blend (x:xs) ys = x:(blend ys xs)
blend _ _ = []

printfFromParamList6 :: String -> [String] -> IO ()
printfFromParamList6 op params = let nAndB = "\t%+5s %+14s\n"
                                     printable = "\tDecimal      Binary\n"
                                                 ++ nAndB
                                                 ++ op
                                                 ++ nAndB
                                                 ++ "------------------------------\n"
                                                 ++ nAndB
                                 in putStrLn $ printf printable ((!!) params 0)
                                                                ((!!) params 1)
                                                                ((!!) params 2)
                                                                ((!!) params 3)
                                                                ((!!) params 4)
                                                                ((!!) params 5)

-- ************************** INDIVIAUAL OPERATION PRINTING METHODS ************************** --

printBinAdd :: Int -> Int -> IO ()
printBinAdd a b = let result = (+) a b
                      params = printBinOp' a b result
                   in printfFromParamList6 "+" params

printBinMinus :: Int -> Int -> IO ()
printBinMinus a b = let result -- a must be greater than b, else flip b and a
                          | a >= b    = (-) a b
                          | otherwise = (-) b a
                        params
                          | a >= b    = printBinOp' a b result
                          | otherwise = printBinOp' b a result
                     in printfFromParamList6 "-" params

-- & (bitwise and)
printBinAnd :: Int -> Int -> IO ()
printBinAnd a b = let result = (.&.) a b
                      params = printBinOp' a b result
                   in printfFromParamList6 "&" params

-- | (bitwise or)
printBinOr :: Int -> Int -> IO ()
printBinOr a b = let result = (.|.) a b
                     params = printBinOp' a b result
                  in printfFromParamList6 "|" params

-- ^ (bitwise XOR)
printBinXOR :: Int -> Int -> IO ()
printBinXOR a b = let result = xor a b
                      params = printBinOp' a b result
                   in printfFromParamList6 "^" params

-- << (left shift)
printBinShiftL :: Int -> Int -> IO ()
printBinShiftL a b = let result = shiftL a b -- shift a left b times
                     in putStrLn $ printf "Decimal:\t%-d << %-d = %-d\n------------------------------\nBinary:%+8s << %-d = %+8s" (a) (b) (result) (toBin a) (b) (toBin result)

-- >> (right shift)
printBinShiftR :: Int -> Int -> IO ()
printBinShiftR a b = let result = shiftR a b -- shift a right b times
                     in putStrLn $ printf "Decimal:\t%-d >> %-d = %-d\n------------------------------\nBinary:%+8s >> %-d = %+8s" (a) (b) (result) (toBin a) (b) (toBin result)

main = do
  putStrLn $ printf "%s\n" "This is BitLib."
