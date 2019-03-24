module Assignment1 (subst, interleave, unroll) where

subst :: Eq t => t -> t -> [t] -> [t]
subst a b [] = []
subst a b (x:xs)
        | x == a = b:subst a b xs
        | otherwise = x:subst a b xs

interleave :: [t] -> [t] -> [t]
interleave [] [] = []
interleave list [] = list
interleave [] list = list
interleave (x:xs) (y:ys) = x:y:interleave xs ys

unroll :: Int -> [a] -> [a]
unroll _ [] = [] 
unroll a list = unrollNext (a - length list) list list

unrollNext :: Int -> [a] -> [a] -> [a]
unrollNext b list (t:ts) 
  | b == 0 = list
  | b > 0 = unrollNext (b-1) (list++[t]) ts2
  | otherwise = unrollNext (b+1) (init list) list
  where ts2 = if length ts == 0 then list else ts  
