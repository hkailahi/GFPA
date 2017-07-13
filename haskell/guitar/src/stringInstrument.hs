import Music

import Data.Maybe
import Data.List
import Data.Monoid
import Control.Applicative
import Control.Monad
import Data.Char

type Frets = Int -- first is open guitar string
-- type instrmntStrings = Int
type Tuning = [PitchClass]


-- Pre-constructed tunings and instruments
ukeStandard = [G, C, E, A] :: Tuning
gtrStandard = [E, A, D, G, B, E] :: Tuning
uK = mkStdUke
gT = mkStdGtr

printChordOnInst :: Chord -> [[PitchClass]] -> IO ()
printChordOnInst c inst = printElements $ (=<<) (\x -> maybePitchToString $ showNoteOccurence x inst) c


showChordToInst :: Chord -> [[PitchClass]] -> [[String]]
showChordToInst [] inst = [[""]]
showChordToInst (x:[]) inst = (maybePitchToString $ showNoteOccurence x inst)
showChordToInst (x:y:[]) inst = (maybePitchToString $ showNoteOccurence x inst) <> (maybePitchToString $ showNoteOccurence y inst)
showChordToInst (x:xs) inst = (maybePitchToString $ showNoteOccurence x inst) <> (showChordToInst xs inst)

printNotes :: [[PitchClass]] -> IO ()
printNotes p = printElements p

printInts :: [[PitchClass]] -> IO ()
printInts p = printElements $ pitchToInt p

printNotePositions :: PitchClass -> [[PitchClass]] -> IO()
printNotePositions p pl = printElements (findNotePositions (toPitchNum p) $ pitchToInt pl)

printOccurences :: PitchClass -> [[PitchClass]] -> IO ()
printOccurences p pl = printElements $ maybePitchToString $ showNoteOccurence p pl

printElements :: (Show a) => [[a]] -> IO ()
printElements = mapM_ print

maybePitchToString :: [[Maybe PitchClass]] -> [[String]]
maybePitchToString b = fmap (fmap (\x -> if (x == Nothing) then " " else show $ fromJust x)) b

pitchToInt :: [[PitchClass]] -> [[Int]]
pitchToInt p = fmap (fmap (toPitchNum)) p

intToPitch :: [[Int]] -> [[PitchClass]]
intToPitch i = fmap (fmap (toPitchClass)) i

mkStdUke :: [[PitchClass]]
mkStdUke = mkIntstrument 22 ukeStandard

mkStdGtr :: [[PitchClass]]
mkStdGtr = mkIntstrument 24 gtrStandard

mkIntstrument :: Frets -> Tuning -> [[PitchClass]]
mkIntstrument f [] = [[]]
mkIntstrument f (x:[]) = [mkString x f]
mkIntstrument f (x:y:[]) = [mkString x f] <> [mkString y f]
mkIntstrument f (x:xs) = [mkString x f] <> (mkIntstrument f xs)

mkString :: PitchClass -> Frets -> [PitchClass]
mkString p f = let pn = toPitchNum p;
               in fmap toPitchClass $ fmap (\x -> mod (x-1) 12) $ fmap (+ pn) [1..f]

findFirstNotePosition :: Int -> [[Int]] -> [Maybe Int]
findFirstNotePosition note uke = fmap (elemIndex note) uke

findNotePositions :: Int -> [[Int]] -> [[Int]]
findNotePositions note uke = fmap (elemIndices note) uke

isNoteOccurence :: PitchClass -> [[PitchClass]] -> [[Bool]]
isNoteOccurence p inst = fmap (fmap ((==) p)) inst

showNoteOccurence :: PitchClass -> [[PitchClass]] -> [[Maybe PitchClass]]
showNoteOccurence p inst = fmap (fmap (\x -> if (p==x) then Just p else Nothing)) inst
