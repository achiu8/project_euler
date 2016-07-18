sumDivisors :: Int -> Int
sumDivisors n = foldl divisible 0 [1..n `div` 2]
  where divisible acc x = if n `mod` x == 0 then acc + x else acc

amicable :: Int -> Bool
amicable n = n == sumDivisors pair && n /= pair
  where pair = sumDivisors n

main = print . sum $ filter amicable [1..9999]
