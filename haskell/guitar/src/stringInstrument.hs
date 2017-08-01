module StringInstrument where

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

data Finger = Index Int | Middle Int | Ring Int | Pinky Int deriving (Show)
type LeftHand = [Finger]

-- Pre-constructed tunings and instruments
ukeStandard = [G, C, E, A] :: Tuning
gtrStandard = [E, A, D, G, B, E] :: Tuning
uK = mkStdUke
gT = mkStdGtr

lH = [(\x -> Index x), (\x -> Middle x), (\x -> Ring x),(\x -> Pinky x)]

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

-- TODO Here lies an abomination, proof of concept
mkFingerOnNote :: [PitchClass] -> [[PitchClass]] -> [Int -> Finger] -> Int -> Int -> [(Int, Finger)] -- int is lh, finger counter, then (string, finger fret)
mkFingerOnNote [] _ _ _ _ = []
mkFingerOnNote (x:[]) inst hand fCntr sCntr = let gStr = sCntr
                                                  fing = (
                                                         (hand <*> [fromJust ((findFirstNotePosition
                                                         (toPitchNum x)
                                                         (pitchToInt inst) !! (gStr)
                                                         ))])
                                                         !! fCntr)
                                              in [(gStr, fing)]
mkFingerOnNote (x:xs) inst hand fCntr sCntr = let gStr = sCntr
                                                  fing = ((hand <*> [fromJust ((findFirstNotePosition (toPitchNum x) (pitchToInt inst) !! (gStr)))]) !! fCntr)
                                              in [(gStr, fing)] ++ (mkFingerOnNote (xs) inst hand (mod (fCntr+1) 4) (mod (sCntr+1) 6))

-- λ: printOccurences C gT
-- [" "," "," "," "," "," "," "," ","C"," "," "," "," "," "," "," "," "," "," "," ","C"," "," "," "]
-- [" "," "," ","C"," "," "," "," "," "," "," "," "," "," "," ","C"," "," "," "," "," "," "," "," "]
-- [" "," "," "," "," "," "," "," "," "," ","C"," "," "," "," "," "," "," "," "," "," "," ","C"," "]
-- [" "," "," "," "," ","C"," "," "," "," "," "," "," "," "," "," "," ","C"," "," "," "," "," "," "]
-- [" ","C"," "," "," "," "," "," "," "," "," "," "," ","C"," "," "," "," "," "," "," "," "," "," "]
-- [" "," "," "," "," "," "," "," ","C"," "," "," "," "," "," "," "," "," "," "," ","C"," "," "," "]
-- λ: let cSong = take 20 $ cycle [C]
-- λ: mkFingerOnNote cSong gT lH 0 0
-- [(0,Index 8),(1,Middle 3),(2,Ring 10),(3,Pinky 5),(4,Index 1),(5,Middle 8),(0,Ring 8),(1,Pinky 3),(2,Index 10),(3,Middle 5),(4,Ring 1),(5,Pinky 8),(0,Index 8),(1,Middle 3),(2,Ring 10),(3,Pinky 5),(4,Index 1),(5,Middle 8),(0,Ring 8),(1,Pinky 3)]

-- printChordOnInst :: Chord -> [[PitchClass]] -> IO ()
-- printChordOnInst c inst = printElements $ (=<<) (\x -> maybePitchToString $ showNoteOccurence x inst) c
--
-- showChordToInst :: Chord -> [[PitchClass]] -> [[String]]
-- showChordToInst [] inst = [[""]]
-- showChordToInst (x:[]) inst = (maybePitchToString $ showNoteOccurence x inst)
-- showChordToInst (x:y:[]) inst = (maybePitchToString $ showNoteOccurence x inst) <> (maybePitchToString $ showNoteOccurence y inst)
-- showChordToInst (x:xs) inst = (maybePitchToString $ showNoteOccurence x inst) <> (showChordToInst xs inst)
