import Data.List (nub)

sumDivisors :: Int -> Int
sumDivisors n = foldl divisible 0 [1..n `div` 2]
  where divisible acc x = if n `mod` x == 0 then acc + x else acc

abundant :: Int -> Bool
abundant n = sumDivisors n > n

allAbundants :: Int -> [Int]
allAbundants n = filter abundant [1..n]

main = do
  let abundants = allAbundants $ 28123 `div` 2
  let abundantSums = nub [ x+y | x <- abundants, y <- abundants, x+y <= 28123 ]
  print . sum $ filter (not . flip elem abundantSums) [1..28123]
