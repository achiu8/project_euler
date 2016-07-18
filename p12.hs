divisors :: Int -> Int
divisors n = foldl divisible 1 [1..n `div` 2]
  where divisible acc x = if n `mod` x == 0 then acc + 1 else acc

triangle :: Int -> Int
triangle n = sum [1..n]

triangles = map triangle [1..]

main = do
  print . head $ dropWhile (\x -> divisors x <= 500) triangles
