module Music where

import Data.List
import Data.Maybe
import Data.Monoid

-- TODO look into reflections, possible localized type class generation at runtime, so we could
-- have proper pitch/chord spellings according to whatKey :: root -> tonality -> key
-- TODO we want data constructors, not typeclasses, so above may not help
-- Maybe we can make smart data constructors - https://wiki.haskell.org/Smart_constructors
data PitchClass = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B deriving (Ord, Enum, Eq, Bounded, Show)

type Chord = [PitchClass]
data ChordType = Maj | Min | Dim | Aug
                 -- Seventh chords
               | Maj7 | Min7 | Dom7 | Dim7 | HDim7 | MinMaj7 | AugMaj7 | Aug7 | Dom7Flat5
                 -- Sixth chords
               | Maj6 | Min6
                 -- Ninth chords
               | Nin | Maj9 | MinMaj9 | Min9 | AugMaj9 | Aug9 | HDim9 | HDimMin9 | Dim9 | DimMin9
                 -- Eleventh chords
               | Elvn | Maj11 | MinMaj11 | Min11 | AugMaj11 | Aug11 | HDim11 |  Dim11
                 -- Thirteenth chords
               | Thrtn | Maj13 | MinMaj13 | Min13 | AugMaj13 | Aug13 | HDim13
                 -- Suspended chords
              --  | Sus4 | Sus2
               deriving (Show, Eq, Enum, Bounded)

type Octave = Int

data Note = Note Pitch Dur | Rest Dur

type Dur = Rational

type Pitch = (PitchClass, Octave)

-- https://en.wikipedia.org/wiki/Chord_names_and_symbols_(popular_music)
mkChord :: PitchClass -> ChordType -> Chord
-- Triads
mkChord p Dim = chordify p [0,3,6]
mkChord p Min = chordify p [0,3,7]
mkChord p Maj = chordify p [0,4,7]
mkChord p Aug = chordify p [0,4,8]
-- Sevenths
mkChord p Maj7 = (mkChord p Maj) <> (chordify p [11])
mkChord p Min7 = (mkChord p Min) <> chordify p [10]
mkChord p Dom7 = (mkChord p Maj) <> chordify p [10]                 -- C7 / Cdom7
mkChord p Dim7 = (mkChord p Dim) <> chordify p [9]
mkChord p HDim7 = (mkChord p Dim) <> chordify p [10]                -- Cm7b5 / Cmin7dim5
mkChord p MinMaj7 = (mkChord p Min) <> chordify p [11]
mkChord p AugMaj7 = (mkChord p Aug) <> chordify p [11]              -- CaugM7 / C+7 / Caug#5
mkChord p Aug7 = (mkChord p Aug) <> chordify p [10]                 -- Caug7
mkChord p Dom7Flat5 = chordify p [0,4,6,10]                         -- C7b5
-- Sixths
mkChord p Maj6 = (mkChord p Maj) <> chordify p [9]                 -- CM6
mkChord p Min6 = (mkChord p Maj) <> chordify p [8]                 -- C6 / Cm6
-- Ninths
mkChord p Nin = (mkChord p Dom7) <> chordify p [2]                -- C9 / Cdom9
mkChord p Maj9 = (mkChord p Maj7) <> chordify p [2]                -- CM9 / CMaj9
mkChord p MinMaj9 = (mkChord p MinMaj7) <> chordify p [2]
mkChord p Min9 = (mkChord p Min7) <> chordify p [2]                -- Cm9 / C min dom 9 / C-9
mkChord p AugMaj9 = (mkChord p AugMaj7) <> chordify p [2]
mkChord p Aug9 = (mkChord p Aug7) <> chordify p [2]
mkChord p HDim9 = (mkChord p HDim7) <> chordify p [2]
mkChord p HDimMin9 = (mkChord p HDim7) <> chordify p [1]          -- C aug 9 / C9 #5
mkChord p Dim9 = (mkChord p Dim7) <> chordify p [2]               -- Cdim9
mkChord p DimMin9 = (mkChord p Dim7) <> chordify p [1]            -- Cdim b9
-- Elevenths
mkChord p Elvn = (mkChord p Nin) <> chordify p [5]
mkChord p Maj11 = (mkChord p Maj9) <> chordify p [5]
mkChord p MinMaj11 = (mkChord p MinMaj9) <> chordify p [5]
mkChord p Min11 = (mkChord p Min9) <> chordify p [5]
mkChord p AugMaj11 = (mkChord p AugMaj9) <> chordify p [5]
mkChord p Aug11 = (mkChord p Aug9) <> chordify p [5]
mkChord p HDim11 = (mkChord p HDimMin9) <> chordify p [5]
mkChord p Dim11 = (mkChord p DimMin9) <> chordify p [4]
-- Thirteenths
mkChord p Thrtn = (mkChord p Elvn) <> chordify p [9]
mkChord p Maj13 = (mkChord p Maj11) <> chordify p [9]
mkChord p MinMaj13 = (mkChord p MinMaj11) <> chordify p [9]
mkChord p Min13 = (mkChord p Min11) <> chordify p [5]
mkChord p AugMaj13 = (mkChord p AugMaj11) <> chordify p [5]
mkChord p Aug13 = (mkChord p Aug11) <> chordify p [5]
mkChord p HDim13 = (mkChord p HDim11) <> chordify p [5]
-- Suspended
-- mkChord p Sus4 = chordify p [99999]
-- mkChord p Sus2 = chordify p [99999]

chordify :: PitchClass -> [Int] -> Chord
chordify p l = fmap (toPitchClass) $ fmap (+ (toPitchNum p)) l

-- fromPitchNum ::
toPitchNum :: PitchClass -> Int
toPitchNum p = fromJust $ elemIndex p [C .. B]

toPitchClass :: Int -> PitchClass
toPitchClass i = (!!) [C .. B] (mod i 12)

mkMajorChord :: PitchClass -> Chord
mkMajorChord p = let pN = toPitchNum p
                 in fmap (toPitchClass) $ fmap (+ pN) [0,4,7]

mkMinorChord :: PitchClass -> Chord
mkMinorChord p = let pN = toPitchNum p
                in fmap (toPitchClass) $ fmap (+ pN) [0,3,7]

-- data Scale = Root [Interval]
-- data Interval = R    -- root
--  | N2   -- b2          1
--  | M2   -- 2           2
--  | N3   -- b3          3
--  | M3   -- 3           4
--  | P4   -- 4           5
--  | Tt   -- b5          6
--  | P5   -- 5           7
--  | I6   -- #5 / b6     8
--  | M6   -- 6           9
--  | I7   -- b7          10
--  | M7   -- 7           11
--  deriving (Ord, Enum, Eq)
