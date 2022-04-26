-- OlhaCh
module Povoden where

plochaVody :: [Int] -> Int
plochaVody xs = sum $ zipWith (-) (zipWith min (scanl1 max xs) (scanr1 max xs)) xs
