scalar :: [Int] -> [Int] -> [Int]
scalar xs ys = zipWith (*) xs ys

add :: [[Int]] -> [[Int]] -> [[Int]]
add xss yss = zipWith (zipWith (+)) xss yss


--mult :: [[Int]] -> [[Int]] -> [[Int]]
--mult xss yss

cart2 :: [t] -> [s] -> [(t,s)]
cart2 xs ys = concat $ map (\x-> map (\y -> (x,y)) ys) xs

--foo :: [[Int]] -> [[Int]] -> [[Int]] 
foo xss yss =  map (sum) $ map (uncurry scalar) 
                (cart2 xss (transpose yss))

goo xss yss =  map (map (sum . uncurry scalar))
                (cart3 xss (transpose yss))
cart3 :: [t] -> [s] -> [[(t,s)]]
cart3 xs ys = map (\x-> map (\y -> (x,y)) ys) xs



-- add [[1,2],[3,4]] [[5,6],[7,8]]
-- cart2 [1,2,3] [4,5,6]
-- mult [[1,2],[3,4]] [[5,6],[7,8]]

