module Proj1 (Pitch, toPitch, feedback,
GameState, initialGuess, nextGuess) where

import Data.List 
data Note = Note Char deriving (Show, Eq, Read)
data Octave = Octave Char deriving (Show, Eq, Read)
data Pitch = Pitch Note Octave deriving (Show, Eq, Read)
type GameState = Int

pitchList :: [Pitch]
pitchList = [Pitch (Note x) (Octave y) | x <- ['A'..'G'], y <- ['1'..'3']]

-- rewrite this bit
combinations :: (Eq t, Num t) => t -> [a] -> [[a]]
combinations 0 lst = [[]]
combinations n lst = do
    (x:xs) <- tails lst
    rest   <- combinations (n-1) xs
    return $ x : rest

getPitchList :: [[Pitch]]
getPitchList = combinations 3 pitchList

isNote :: Char -> Bool
isNote x = x == 'A' || x == 'B' || x == 'C' || x == 'D' || x == 'E' || x == 'F' || x == 'G'

isOctave :: Char -> Bool
isOctave x = x == '1' || x == '2' || x == '3'

toPitch :: String -> Maybe Pitch
toPitch [] = Nothing
toPitch [x, y] = if isNote x && isOctave y then Just (Pitch (Note x) (Octave y)) else Nothing

countCorrectPitch :: [String] -> [String] -> Int
countCorrectPitch target [x] = if x `elem` target then 1 else 0
countCorrectPitch target (x:xs) = countCorrectPitch target [x] + countCorrectPitch target xs

toNode :: [String] -> [Char]
toNode [] = []
toNode (x:xs) = head x:toNode xs

countTrue :: [Bool] -> Int
countTrue [] = 0
countTrue (x:xs) = if x then 1+countTrue xs else 0+countTrue xs

countCorrectNote :: [String] -> [String] -> Int
countCorrectNote arr1 arr2 = countTrue (map (`elem` (toNode arr1)) (toNode arr2))

toOctave :: [String] -> [Char]
toOctave [] = []
toOctave (x:xs) = tail x ++ toOctave xs

countCorrectOctave :: [Char] -> [Char] -> Int
countCorrectOctave [] _ = 0
countCorrectOctave _ [] = 0
countCorrectOctave (x:xs) (y:ys) = if (x == y) then 1+countCorrectOctave xs ys else countCorrectOctave (x:xs) ys

pitchesToStrings :: [Pitch] -> [String]
pitchesToStrings [] = []
pitchesToStrings (x:xs) = pitchToString x:pitchesToStrings xs

pitchToString :: Pitch -> String
pitchToString (Pitch (Note x) (Octave y)) = x:[y]


-- add toPitch to this function
feedback :: [Pitch] -> [Pitch] -> (Int,Int,Int)
feedback _ [] = (0,0,0)
feedback [] _ = (0,0,0)
feedback arr1 arr2 = 
    (countCorrectPitch l1 l2, countCorrectNote l1 l2 - countCorrectPitch l1 l2, countCorrectOctave (toOctave l1) (toOctave l2) - countCorrectPitch l1 l2)
    where l1 = pitchesToStrings arr1
          l2 = pitchesToStrings arr2

initialGuess :: ([Pitch],GameState)
initialGuess = ([(Pitch (Note 'A') (Octave '1')), (Pitch (Note 'A') (Octave '2')), (Pitch (Note 'A') (Octave '1'))], 0)

nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch],GameState)
nextGuess (input, index) feedbackInput = (getPitchList !! index, index + 1)


