module Proj1 (Pitch, toPitch, feedback,
GameState, initialGuess, nextGuess) where

import Data.List 
data Note = Note Char deriving (Show, Eq, Read)
data Octave = Octave Char deriving (Show, Eq, Read)
data Pitch = Pitch Note Octave deriving (Show, Eq, Read)
type GameState = ([[Pitch]],[Pitch])

pitchList :: [Pitch]
pitchList = makePitchList ['A'..'G'] ['1'..'3']

makePitchList :: [Char] -> [Char] -> [Pitch]
makePitchList notes [] = [Pitch (Note x) (Octave y) | x <- notes,  y <- ['1'..'3']]
makePitchList [] octaves = [Pitch (Note x) (Octave y) | x <- ['A'..'G'],  y <- octaves]
makePitchList notes octaves = [Pitch (Note x) (Octave y) | x <- notes, y <- octaves]

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

toNote :: [String] -> [Char]
toNote [] = []
toNote (x:xs) = nub $ head x:toNote xs

countCorrect :: [Char] -> [Char] -> Int
countCorrect _ [] = 0
countCorrect target (x:xs) = if x `elem` target then 1 + countCorrect target xs else countCorrect target xs

toOctave :: [String] -> [Char]
toOctave [] = []
toOctave (x:xs) = nub (tail x ++ toOctave xs)

pitchesToStrings :: [Pitch] -> [String]
pitchesToStrings [] = []
pitchesToStrings (x:xs) = pitchToString x:pitchesToStrings xs

pitchToString :: Pitch -> String
pitchToString (Pitch (Note x) (Octave y)) = x:[y]

-- carefully examine this function -> it might be wrong!
feedback :: [Pitch] -> [Pitch] -> (Int,Int,Int)
feedback _ [] = (0,0,0)
feedback [] _ = (0,0,0)
feedback arr1 arr2 = 
    if (countCorrectPitch l1 l2) == 3 then (3, 0, 0) else
    (
      countCorrectPitch l1 l2, 
      countCorrect (toNote l1) (toNote l2) - countCorrectPitch l1 l2, 
      countCorrect (toOctave l1) (toOctave l2) - countCorrectPitch l1 l2
    )
    where l1 = pitchesToStrings arr1
          l2 = pitchesToStrings arr2

initialGuess :: ([Pitch],GameState)
initialGuess = ([
  (Pitch (Note 'A') (Octave '1')), 
  (Pitch (Note 'A') (Octave '2')), 
  (Pitch (Note 'A') (Octave '3'))], (getPitchList,[]))

getUpdatedRemoveList :: [Pitch] -> [Pitch] -> (Int, Int, Int) -> [Pitch]
getUpdatedRemoveList input removeList feedbackInput = 
  case feedbackInput of (0, 0, 0) -> nub (removeList ++ (handleTripleZeroCase input))
                        (0, 0, _) -> nub (removeList ++ (handleZeroNote input))
                        (0, _, 0) -> nub (removeList ++ (handleZeroOctave input))
                        (0, _, _) -> nub (removeList ++ input)
                        feedbackInput -> removeList

nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch],GameState)
nextGuess (input, (arr, removeList)) feedbackInput = 
    let updatedRemoveList = getUpdatedRemoveList input removeList feedbackInput
        updatedArr = jumpToNonRemovePitch arr updatedRemoveList
    in ((head updatedArr), ((tail updatedArr), updatedRemoveList))

jumpToNonRemovePitch :: [[Pitch]] -> [Pitch] -> [[Pitch]]
jumpToNonRemovePitch arr rm = 
  if all (==False) (map (`elem` (head arr)) rm) then arr else jumpToNonRemovePitch (tail arr) rm


-- can prob merge these to one
handleTripleZeroCase :: [Pitch] -> [Pitch]
handleTripleZeroCase [] = []
handleTripleZeroCase (x:xs) = nub (generateRemoveItems x ++ handleTripleZeroCase xs)

handleZeroNote :: [Pitch] -> [Pitch]
handleZeroNote [] = []
handleZeroNote (x:xs) = generateRemoveItemsByNote x ++ handleZeroNote xs

handleZeroOctave :: [Pitch] -> [Pitch]
handleZeroOctave [] = []
handleZeroOctave (x:xs) = generateRemoveItemsByOctave x ++ handleZeroOctave xs

generateRemoveItemsByNote :: Pitch -> [Pitch]
generateRemoveItemsByNote (Pitch (Note n) _) = nub ((makePitchList [n] []))

generateRemoveItemsByOctave :: Pitch -> [Pitch]
generateRemoveItemsByOctave (Pitch _ (Octave o)) = nub ((makePitchList [] [o]))

generateRemoveItems :: Pitch -> [Pitch]
generateRemoveItems (Pitch (Note n) (Octave o)) = nub ((makePitchList [n] []) ++ (makePitchList [] [o]))
