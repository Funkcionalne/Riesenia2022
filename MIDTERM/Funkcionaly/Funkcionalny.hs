-- OlhaCh

module Funkcionalny where

import Data.List (transpose)

scalar :: [Int] -> [Int] -> Int
scalar m n = sum $ zipWith (*) m n

add :: [[Int]] -> [[Int]] -> [[Int]]
add = zipWith (zipWith (+))

cart2 :: [t] -> [s] -> [(t, s)]
cart2 ms ns = foldl (\acc m -> acc ++ foldl (\acc' n -> acc' ++ [(m, n)]) [] ns) [] ms

mult :: [[Int]] -> [[Int]] -> [[Int]]
mult m n = chunksOf (length m) $ foldl (\acc x -> acc ++ [uncurry scalar x]) [] (cart2 m (transpose n))

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = takeWhile (not . null) . map (take n) . iterate (drop n)
