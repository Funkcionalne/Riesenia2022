module Povoden where

plochaVody :: [Int] -> Int
plochaVody pohorie = sum [ let m = min (z1!!i) (z2!!i) in 
                        if m > (pohorie!!i) 
                        then m-(pohorie!!i) 
                        else 0 
                     | i <- [0..length z1-1] 
                     ]
                     where z1 :: [Int]
                           z1 = init (scanl (max) (minBound::Int) pohorie)
                           z2 :: [Int]
                           z2 = tail (reverse ( scanl (max) (minBound::Int) (reverse pohorie)))

{-
plochaVody [1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1] == 6
plochaVody [2,1,2] == 1
plochaVody [3, 0, 1, 3, 0, 5] == 8
plochaVody [3,0,0,2,0,4] == 10
plochaVody [1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1] == 6
-}