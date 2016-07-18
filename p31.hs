ways :: Int -> [Int] -> Int
ways 0 _ = 1
ways _ [] = 0
ways n (c:coins) = sum [ ways (n - (x*c)) coins | x <- [0..n `div` c] ]

main = print $ ways 200 [1,2,5,10,20,50,100,200]
