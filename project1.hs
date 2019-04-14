module Proj1 (Pitch, toPitch, feedback,
GameState, initialGuess, nextGuess) where

import Data.List 
data Note = Note Char deriving (Show, Eq, Read)
data Octave = Octave Char deriving (Show, Eq, Read)
data Pitch = Pitch Note Octave deriving (Show, Eq, Read)
type GameState = ([[Pitch]],[Pitch], Bool, [Pitch], [Pitch])

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

getCorrectPitches :: [String] -> [String] -> [String]
getCorrectPitches target [] = []
getCorrectPitches target (x:xs) = if x `elem` target then x:getCorrectPitches target xs else getCorrectPitches target xs

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

removeFromList :: [String] -> [String] -> [String]
removeFromList target [] = []
removeFromList target (x:xs) = if x `elem` target then removeFromList target xs else x:removeFromList target xs

feedback :: [Pitch] -> [Pitch] -> (Int,Int,Int)
feedback _ [] = (0,0,0)
feedback [] _ = (0,0,0)
feedback arr1 arr2 = 
    (
      length correctPitches,
      countCorrect (toNote updatedL1) (toNote updatedL2),
      countCorrect (toOctave updatedL1) (toOctave updatedL2)
    )
    where l1 = pitchesToStrings arr1
          l2 = pitchesToStrings arr2
          correctPitches = getCorrectPitches l1 l2
          updatedL1 = (removeFromList correctPitches l1)
          updatedL2 = (removeFromList correctPitches l2)

initialGuess :: ([Pitch],GameState)
initialGuess = (initG, (getPitchList,[], False, initG, []))
  where initG = [Pitch (Note 'A') (Octave '1'), Pitch (Note 'A') (Octave '2'), Pitch (Note 'A') (Octave '3')]

getUpdatedRemoveList :: [Pitch] -> [Pitch] -> [Pitch] -> (Int, Int, Int) -> [Pitch]
getUpdatedRemoveList input removeList correctList (_, y, z)
  | qualified = nub (removeList ++ toRemovePitch)
  | otherwise = removeList
  where toRemovePitch = removeDup correctList input
        qualified = length toRemovePitch < 3
  
getUpdatedRemoveListZero :: [Pitch] -> [Pitch] -> (Int, Int, Int) -> [Pitch]
getUpdatedRemoveListZero input removeList feedbackInput = 
  case feedbackInput of (0, 0, 0) -> nub (removeList ++ (handleTripleZeroCase input))
                        (0, 0, _) -> nub (removeList ++ (handleZeroNote input))
                        (0, _, 0) -> nub (removeList ++ (handleZeroOctave input))
                        (0, _, _) -> nub (removeList ++ input)
                        feedbackInput -> removeList

getSamePitches :: [Pitch] -> [Pitch] -> [Pitch]
getSamePitches [] _ = []
getSamePitches (x:xs) prevInput = 
  if x `elem` prevInput then x:getSamePitches xs prevInput else getSamePitches xs prevInput

removeDup :: [Pitch] -> [Pitch] -> [Pitch]
removeDup _ [] = []
removeDup input (x:xs) = if x `elem` input then removeDup input xs else x:removeDup input xs

handleAfterTwoZeroZero :: [Pitch] -> [Pitch] -> (Int, Int, Int) -> [Pitch]
handleAfterTwoZeroZero input prevInput feedbackInput
  -- | isOne && (length getSame == 2) = removeDup input prevInput
  | isOne && (length getSame == 1) = getSame
  -- | isTwo && ((length $ getSame) > 0) = getSame
  | otherwise = []
  where getSame = getSamePitches input prevInput
        isOne = fst3 feedbackInput == 1
        isTwo = fst3 feedbackInput == 2

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch],GameState)
nextGuess (input, (arr, removeList, previs2xx, prevInput, correctList)) feedbackInput = 
    let separateHandle = length correctList == fst3 feedbackInput && length correctList > 0
        updatedRemoveList = if separateHandle 
          then getUpdatedRemoveList input removeList correctList feedbackInput
          else getUpdatedRemoveListZero input removeList feedbackInput
        is2xx = fst3 feedbackInput == 2
        updatedCorrectList = if previs2xx then 
          nub $ handleAfterTwoZeroZero input prevInput feedbackInput ++ correctList
          else correctList
        updatedArr = jumpToNonRemovePitch arr updatedRemoveList correctList
    in ((head updatedArr), ((tail updatedArr), updatedRemoveList, is2xx, input, updatedCorrectList))

jumpToNonRemovePitch :: [[Pitch]] -> [Pitch] -> [Pitch] -> [[Pitch]]
jumpToNonRemovePitch arr rm cor = 
  if all (==False) (map (`elem` (head arr)) rm) 
    && all (==True) (map (`elem` (head arr)) cor) 
    then arr 
    else jumpToNonRemovePitch (tail arr) rm cor


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
