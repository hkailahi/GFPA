# Guitar CLI App

NOTE: readme is WIP

## Description

Simple CLI app that locates and prints notes on an ASCII guitar fretboard. This code's purpose was as a proof-of-concept. Further work is being done in my Haskell project here.

## Terminology

Some necessary terms are either overloaded, or have different meanings in music and programming. For the sake of simplicity and explanation:

- Cell : a box/coordinate on the fretboard containing a string number and a fret number (position)
- String : a physical guitar string, and not a text type
- Note : a sign used to denote a pitch from twelve-tone equal temperment
  - we are ignoring duration of the pitch
- Key : the group of pitches, or scale upon which a song/piece is written
  - we are only using major and minor modalities

| Note  | Number |
| :---: | :---: |
| C     | 0 |
| C#/Db | 1 |
| D     | 2 |
| D#/Eb | 3 |
| E     | 4 |
| F     | 5 |
| F#/Gb | 6 |
| G     | 7 |
| G#/Ab | 8 |
| A     | 9 |
| A#/Bb | 10 |
| B     | 11 |

## App

There are four functionalities:

1. Display notes on fretboard
2. Display notes on fretboard as integers
3. Display octave numbers on fretboard
4. Display notes in a given key
  - Major and natural minor key signatures from the circle of fifths
  - Given an input key (ex: C Major/Bb Minor), it will show the cells on fretboard that correspond to input scale.

## Caveats
- Assumes standard tuning, though it should be simple enough to change.
- Assumes 12 frets (plus open notes) and 6 strings. This fret count should not be a problem as the notes on a guitar fretboard repeat after 12 cells.
