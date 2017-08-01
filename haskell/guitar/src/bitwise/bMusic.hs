module BMusic where

import BitLib
import Data.List
import Music
import Data.Bits
import Control.Monad

type Scale = Int  -- 12 bit and 11 choose [1..11]
type BitChord = Int  -- 12 bit and 11 choose [1..11]

-- a number contained in pitchList
type Pitch = Int  -- 12 bit and 12 choose 1

data ScaleType = Major
               | HarmMinor | NatMinor | MelMinor
               | Chromatic

-- B    As    A   ..  C
-- 2^0  2^1   2^2 ..  2^11
pitchList :: [Int]
pitchList = reverse $ fmap (\x -> 2^x) [0..11]

mkScale :: PitchClass -> ScaleType -> Scale
-- Triads
mkScale p Major     = bchordify p [0,2,4,5,7,9,11]  -- 101011010101 1 2  3 4 5  6  7
mkScale p NatMinor  = bchordify p [0,2,3,5,7,8,10]  -- 101101011010 1 2 b3 4 5 b6 b7 -- Desceding melodic == NatMinor
mkScale p HarmMinor = bchordify p [0,2,3,5,7,8,11]  -- 101101011001 1 2 b3 4 5 b6  7
mkScale p MelMinor  = bchordify p [0,2,3,5,7,9,10]  -- 101101010101 1 2 b3 4 5  6  7 -- Ascending melodic
mkScale p Chromatic = bchordify p [0 .. 11]         -- 111111111111 1 b2 2 b3 3 4 b5 5 b6 6 b7 7

bchordify :: PitchClass -> [Int] -> BitChord
bchordify p l = let rootPos = 11 - toPitchNum p
                    pitches = fmap (\x -> 2 ^ (mod (rootPos - x) 12)) l
                in foldl (.|.) 0 pitches

rotateList :: Int -> [a] -> [a]
rotateList _ [] = []
rotateList n xs = zipWith const (drop n (cycle xs)) xs

orderedNoteNames :: Int -> PitchClass -> [PitchClass]
orderedNoteNames n p = let rootPos    = toPitchNum p
                           ordPitches = rotateList rootPos [C .. B]
                           ordScale   = rotateList rootPos (matchZeroes 2048 n)
                           fromBit    = (\bit note -> if bit == 1 then [note] else [])
                       in join $ zipWith (fromBit) ordScale ordPitches

btoNoteName :: Int -> [PitchClass]
btoNoteName n = join $ zipWith (\bit note -> if bit == 1 then [note] else []) (matchZeroes 2048 n) [C .. B]

mkBitChord :: PitchClass -> ChordType -> BitChord
-- Triads
mkBitChord p Dim = bchordify p [0,3,6]
mkBitChord p Min = bchordify p [0,3,7]
mkBitChord p Maj = bchordify p [0,4,7]
mkBitChord p Aug = bchordify p [0,4,8]
-- Sevenths
mkBitChord p Maj7 = (mkBitChord p Maj) .|. (bchordify p [11])
mkBitChord p Min7 = (mkBitChord p Min) .|. bchordify p [10]
mkBitChord p Dom7 = (mkBitChord p Maj) .|. bchordify p [10]                 -- C7 / Cdom7
mkBitChord p Dim7 = (mkBitChord p Dim) .|. bchordify p [9]
mkBitChord p HDim7 = (mkBitChord p Dim) .|. bchordify p [10]                -- Cm7b5 / Cmin7dim5
mkBitChord p MinMaj7 = (mkBitChord p Min) .|. bchordify p [11]
mkBitChord p AugMaj7 = (mkBitChord p Aug) .|. bchordify p [11]              -- CaugM7 / C+7 / Caug#5
mkBitChord p Aug7 = (mkBitChord p Aug) .|. bchordify p [10]                 -- Caug7
mkBitChord p Dom7Flat5 = bchordify p [0,4,6,10]                         -- C7b5
-- Sixths
mkBitChord p Maj6 = (mkBitChord p Maj) .|. bchordify p [9]                 -- CM6
mkBitChord p Min6 = (mkBitChord p Maj) .|. bchordify p [8]                 -- C6 / Cm6
-- Ninths
mkBitChord p Nin = (mkBitChord p Dom7) .|. bchordify p [2]                -- C9 / Cdom9
mkBitChord p Maj9 = (mkBitChord p Maj7) .|. bchordify p [2]                -- CM9 / CMaj9
mkBitChord p MinMaj9 = (mkBitChord p MinMaj7) .|. bchordify p [2]
mkBitChord p Min9 = (mkBitChord p Min7) .|. bchordify p [2]                -- Cm9 / C min dom 9 / C-9
mkBitChord p AugMaj9 = (mkBitChord p AugMaj7) .|. bchordify p [2]
mkBitChord p Aug9 = (mkBitChord p Aug7) .|. bchordify p [2]
mkBitChord p HDim9 = (mkBitChord p HDim7) .|. bchordify p [2]
mkBitChord p HDimMin9 = (mkBitChord p HDim7) .|. bchordify p [1]          -- C aug 9 / C9 #5
mkBitChord p Dim9 = (mkBitChord p Dim7) .|. bchordify p [2]               -- Cdim9
mkBitChord p DimMin9 = (mkBitChord p Dim7) .|. bchordify p [1]            -- Cdim b9
-- Elevenths
mkBitChord p Elvn = (mkBitChord p Nin) .|. bchordify p [5]
mkBitChord p Maj11 = (mkBitChord p Maj9) .|. bchordify p [5]
mkBitChord p MinMaj11 = (mkBitChord p MinMaj9) .|. bchordify p [5]
mkBitChord p Min11 = (mkBitChord p Min9) .|. bchordify p [5]
mkBitChord p AugMaj11 = (mkBitChord p AugMaj9) .|. bchordify p [5]
mkBitChord p Aug11 = (mkBitChord p Aug9) .|. bchordify p [5]
mkBitChord p HDim11 = (mkBitChord p HDimMin9) .|. bchordify p [5]
mkBitChord p Dim11 = (mkBitChord p DimMin9) .|. bchordify p [4]
-- Thirteenths
mkBitChord p Thrtn = (mkBitChord p Elvn) .|. bchordify p [9]
mkBitChord p Maj13 = (mkBitChord p Maj11) .|. bchordify p [9]
mkBitChord p MinMaj13 = (mkBitChord p MinMaj11) .|. bchordify p [9]
mkBitChord p Min13 = (mkBitChord p Min11) .|. bchordify p [5]
mkBitChord p AugMaj13 = (mkBitChord p AugMaj11) .|. bchordify p [5]
mkBitChord p Aug13 = (mkBitChord p Aug11) .|. bchordify p [5]
mkBitChord p HDim13 = (mkBitChord p HDim11) .|. bchordify p [5]

-- bmkMajorChord :: PitchClass -> BitChord
-- bmkMajorChord p = let rootPos = 11 - toPitchNum p
--                       root  = 2 ^ rootPos
--                       third = 2 ^ (mod (rootPos - 4) 12)
--                       fifth = 2 ^ (mod (rootPos - 7) 12)
--                   in root .|. third .|. fifth
--
-- bmkMinChord :: PitchClass -> BitChord
-- bmkMinChord p = let rootPos = 11 - toPitchNum p
--                     root  = 2 ^ rootPos
--                     third = 2 ^ (mod (rootPos - 3) 12)
--                     fifth = 2 ^ (mod (rootPos - 7) 12)
--                 in root .|. third .|. fifth
