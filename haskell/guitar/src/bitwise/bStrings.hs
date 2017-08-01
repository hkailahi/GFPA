module BStrings where

import Music
import BMusic
import StringInstrument
import BitLib

import Data.Monoid
import Control.Monad

mkBitInstrument :: Frets -> Tuning -> [Int]
mkBitInstrument f []     =  []
mkBitInstrument f (x:[]) = [mkBitString f]
mkBitInstrument f (x:xs) = [mkBitString f] <> (mkBitInstrument f xs)

mkBitString :: Frets -> Int
mkBitString f = fromBinDigits $ take f $ cycle [1]

-- PitchClass is tuning[i] here
-- scaleOnString :: [Int] -> Tuning -> PitchClass -> Scale -> ScaleType -> [Int]
-- scaleOnString gtr tuning root s sT =  .&. rotateR12 toBinDigits (mkScale root sT)

bitGtr = mkBitInstrument 22 gtrStandard
bitUke = mkBitInstrument 12 ukeStandard

scaleOnInst :: [Int] -> Frets -> Tuning -> PitchClass -> ScaleType -> [PitchClass]
scaleOnInst inst f t root sT = let scale      = mkScale root sT
                                   rootPos    = toPitchNum root
                                   notes      = orderedNoteNames (scale) root
                                   fromBit    = (\bit note -> if bit == 1 then [note] else [As]) -- 'As' is filler for not in scale note
                                   ordPitches = take f $ cycle $ rotateList rootPos [C .. B]
                                   ordScale   = take f $ cycle $ rotateList rootPos (matchZeroes 2048 scale)
                                in join $ zipWith (fromBit) ordScale ordPitches

-- orderedNoteNames :: Int -> PitchClass -> [PitchClass]
-- orderedNoteNames n p = let rootPos    = toPitchNum p
--                            ordPitches = rotateList rootPos [C .. B]
--                            ordScale   = rotateList rootPos (matchZeroes 2048 n)
--                            fromBit    = (\bit note -> if bit == 1 then [note] else [])
--                        in join $ zipWith (fromBit) ordScale ordPitches
