-- Author: Wenyen Wei <wenyenw@student.unimelb.edu.au>
-- Login Id: 949624

-- Purpose: The objective of this project is to practice and assess my 
-- understanding of functional programming and Haskell. This project is to 
-- implement both the guessing and answering parts of a logical guessing game.

-- The program: This is a Musician game, one player is the composer and the 
-- other is the performer. The composer begins by selecting a three-pitch
-- musical chord, where each pitch comprises a musical note, one of A, B,
-- C, D, E, F, or G, and an octave, one of 1, 2, or 3. The player continues
-- guessing until they got the right pitches.

module Proj1 (Pitch, toPitch, feedback,
GameState, initialGuess, nextGuess) where

import Data.List 
data Pitch = Pitch {note :: Char, octave :: Char}
instance Show Pitch where
  show Pitch {note = x, octave = y} = [x,y]
instance Eq Pitch where
  Pitch {note = x, octave = y} == Pitch {note = a, octave = b} = True
  _ == _ = False
-- GameState is a list of remaining 3-Pitche combinations
type GameState = ([[Pitch]])

toPitch :: String -> Maybe Pitch
toPitch [] = Nothing
toPitch [x, y] = if isNote x && isOctave y 
  then Just Pitch {note = x, octave = y} else Nothing

-- "pitchList", "combinations", "getPitchList" are all functions/supporting
-- functions for generating pitch combinations from A1 ... G3

pitchList :: [Pitch]
pitchList = [Pitch x y | x <- ['A'..'G'], y <- ['1'..'3']]

combinations :: (Eq t, Num t) => t -> [a] -> [[a]]
combinations 0 arr = [[]]
combinations n arr = do
    (x:xs) <- tails arr
    remain   <- combinations (n-1) xs
    return ([x] ++ remain)

getPitchList :: [[Pitch]]
getPitchList = combinations 3 pitchList


-- "isNote", "isOctave", "toNote", "toOctave" are functions to validate/
-- transform pitches these are support functions for "feedback" function

getNote :: Pitch -> Char
getNote (Pitch {note = x, octave = y}) = x

getOctave :: Pitch -> Char
getOctave (Pitch {note = x, octave = y}) = y

isNote :: Char -> Bool
isNote x = 
  x == 'A' || x == 'B' || x == 'C' || x == 'D' 
  || x == 'E' || x == 'F' || x == 'G'

isOctave :: Char -> Bool
isOctave x = x == '1' || x == '2' || x == '3'

toNote :: [Pitch] -> [Char]
toNote [] = []
toNote (x:xs) = getNote x:toNote xs

toOctave :: [Pitch] -> [Char]
toOctave [] = []
toOctave (x:xs) = getOctave x:toOctave xs

-- "getCorrectPitches", "countCorrect", counts the correct Pitches/Note/Octave
-- of a Pitch combination. These are support functions for "feedback" function 

-- "getCorrectPitches" count the common pitches between the target and guess
getCorrectPitches :: [Pitch] -> [Pitch] -> [Pitch]
getCorrectPitches target [] = []
getCorrectPitches target (x:xs) = 
  if x `elem` target 
    then x:getCorrectPitches target xs 
    else getCorrectPitches target xs

removeItemFromArr :: Char -> [Char] -> [Char]
removeItemFromArr item [] = []
removeItemFromArr item (x:xs) = if x == item 
  then xs else x:removeItemFromArr item xs

-- "countCorrect" count the common Notes/Octaves between the target and guess
countCorrect :: [Char] -> [Char] -> Int
countCorrect _ [] = 0
countCorrect [] _ = 0
countCorrect (t:ts) arr = if t `elem` arr 
  then 1 + countCorrect ts (removeItemFromArr t arr)
  else countCorrect ts arr

-- "pitchesToStrings", "pitchToString", "removeFromList" are support 
-- functions for "feedback" function

pitchesToStrings :: [Pitch] -> [String]
pitchesToStrings [] = []
pitchesToStrings (x:xs) = pitchToString x:pitchesToStrings xs

pitchToString :: Pitch -> String
pitchToString (Pitch x y) = x:[y]

removeFromList :: [Pitch] -> [Pitch] -> [Pitch]
removeFromList target [] = []
removeFromList target (x:xs) = 
  if x `elem` target 
    then removeFromList target xs 
    else x:removeFromList target xs

-- "feedback" first use "getCorrectPitches" function and then remove the 
-- correct pitches from the list as shown in updateL1/updateL2.
-- "countCorrect" then use the updated list to process counting the 
-- correct Octave/Notes.
feedback :: [Pitch] -> [Pitch] -> (Int,Int,Int)
feedback _ [] = (0,0,0)
feedback [] _ = (0,0,0)
feedback arr1 arr2 = 
    (
      length correctPitches,
      countCorrect (toNote updatedL1) (toNote updatedL2),
      countCorrect (toOctave updatedL1) (toOctave updatedL2)
    )
    where l1 = arr1
          l2 = arr2
          correctPitches = getCorrectPitches l1 l2
          updatedL1 = (removeFromList correctPitches l1)
          updatedL2 = (removeFromList correctPitches l2)

initialGuess :: ([Pitch],GameState)
initialGuess = ([
  Pitch 'A' '1', 
  Pitch 'A' '2', 
  Pitch 'A' '3'], getPitchList)

nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch],GameState)
nextGuess (input, arr) feedbackInput =
  let updatedArr = jumpToNonRemovePitch arr input feedbackInput
  in ((head updatedArr), (tail updatedArr))

-- the strategy of "jumpToNonRemovePitch" is to remove symmetry in the 
-- problem space.

jumpToNonRemovePitch :: [[Pitch]] -> [Pitch] -> (Int, Int, Int) -> [[Pitch]]
jumpToNonRemovePitch arr input feedbackInput = 
  if feedback input (head arr) == feedbackInput 
    then arr 
    else jumpToNonRemovePitch (tail arr) input feedbackInput
