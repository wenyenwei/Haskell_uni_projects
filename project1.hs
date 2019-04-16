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

data Pitch = Pitch {note :: Char, octave :: Char} deriving (Eq)
instance Show Pitch where
  show Pitch {note = x, octave = y} = [x,y]

-- GameState is a list of remaining 3-Pitche combinations
type GameState = ([[Pitch]])

toPitch :: String -> Maybe Pitch
toPitch [] = Nothing
toPitch [x, y] = if isNote x && isOctave y 
  then Just Pitch {note = x, octave = y} else Nothing

-- "pitchList" creates all the pitches from A1 ... G3
pitchList :: [Pitch]
pitchList = [Pitch x y | x <- ['A'..'G'], y <- ['1'..'3']]

-- "combinations" make combinations from a list of a certain number.
-- e.g. combinations 2 [1,2,3] = [[1,2],[2,3],[1,3]]
combinations :: (Eq t, Num t) => t -> [a] -> [[a]]
combinations 0 arr = [[]]
combinations n arr = do
    (x:xs) <- tails arr
    remain   <- combinations (n-1) xs
    return ([x] ++ remain)

-- "getPitchList" use "pitchList" and "combinations" to create all 1330 
-- Pitch combinations
getPitchList :: [[Pitch]]
getPitchList = combinations 3 pitchList

-- "getNote", "getOctave" are getter functions for data type Pitch 
-- to get Note/Octave from Pitch
getNote :: Pitch -> Char
getNote (Pitch {note = x, octave = y}) = x

getOctave :: Pitch -> Char
getOctave (Pitch {note = x, octave = y}) = y


-- "isNote", "isOctave" validate a Char is a valid Note/Octave
isNote :: Char -> Bool
isNote x = 
  x == 'A' || x == 'B' || x == 'C' || x == 'D' 
  || x == 'E' || x == 'F' || x == 'G'

isOctave :: Char -> Bool
isOctave x = x == '1' || x == '2' || x == '3'

-- "toNote", "toOctave" transform a list of Pitches to a list of Note/Octave
toNote :: [Pitch] -> [Char]
toNote [] = []
toNote (x:xs) = getNote x:toNote xs

toOctave :: [Pitch] -> [Char]
toOctave [] = []
toOctave (x:xs) = getOctave x:toOctave xs

-- "getCorrectPitches" count the common pitches between the target and guess
getCorrectPitches :: [Pitch] -> [Pitch] -> [Pitch]
getCorrectPitches target [] = []
getCorrectPitches target (x:xs) = 
  if x `elem` target 
    then x:getCorrectPitches target xs 
    else getCorrectPitches target xs

-- "countCorrect" count the common Notes/Octaves between the target and guess
countCorrect :: [Char] -> [Char] -> Int
countCorrect _ [] = 0
countCorrect [] _ = 0
countCorrect (t:ts) arr = if t `elem` arr 
  then 1 + countCorrect ts (removeItemFromArr t arr)
  else countCorrect ts arr

-- "removeFromList" and "removeItemFromArr" are support functions for "feedback" function
-- if input contains element in target, remove the element from input
removeFromList :: [Pitch] -> [Pitch] -> [Pitch]
removeFromList target [] = []
removeFromList target (x:xs) = 
  if x `elem` target 
    then removeFromList target xs 
    else x:removeFromList target xs

-- "removeItemFromArr" removes a single target item from an array
removeItemFromArr :: Char -> [Char] -> [Char]
removeItemFromArr item [] = []
removeItemFromArr item (x:xs) = if x == item 
  then xs else x:removeItemFromArr item xs

-- "feedback" first use "getCorrectPitches" function and then remove the 
-- correct pitches from the list as shown in updateL1/updateL2.
-- "countCorrect" then use the updated list to process counting the 
-- correct Octave/Notes.
feedback :: [Pitch] -> [Pitch] -> (Int,Int,Int)
feedback _ [] = (0,0,0)
feedback [] _ = (0,0,0)
feedback l1 l2 = 
    (
      length correctPitches,
      countCorrect (toNote updatedL1) (toNote updatedL2),
      countCorrect (toOctave updatedL1) (toOctave updatedL2)
    )
    where correctPitches = getCorrectPitches l1 l2
          updatedL1 = (removeFromList correctPitches l1)
          updatedL2 = (removeFromList correctPitches l2)

initialGuess :: ([Pitch],GameState)
initialGuess = ([
  Pitch 'A' '1', 
  Pitch 'B' '2', 
  Pitch 'C' '3'], getPitchList)

nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch],GameState)
nextGuess (input, arr) feedbackInput =
  let updatedArr = filterPitch arr input feedbackInput
  in ((head updatedArr), (tail updatedArr))

-- the strategy of "filterPitch" is to remove symmetry in the 
-- problem space.

filterPitch :: [[Pitch]] -> [Pitch] -> (Int, Int, Int) -> [[Pitch]]
filterPitch [] _ _ = []
filterPitch (x:xs) input feedbackInput = if (feedback input x == feedbackInput) 
  then x:filterPitch xs input feedbackInput
  else filterPitch xs input feedbackInput
