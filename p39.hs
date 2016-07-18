triangles :: Int -> Int
triangles n = length [ (a,b,c) | a <- [1..n], b <- [1..a], c <- [1..n], a*a + b*b == c*c, a+b+c == n ]

maxSolutions :: (Int,Int) -> Int -> (Int, Int)
maxSolutions (a,aCount) b
  | aCount >= bCount = (a,aCount)
  | otherwise = (b,bCount)
  where bCount = triangles b

main = print $ foldl maxSolutions (0,0) [1..1000]
