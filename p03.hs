primeFactors :: Int -> [Int]
primeFactors n = primeFactors' n 2
  where primeFactors' 1 _ = []
        primeFactors' x d
          | x `mod` d == 0 = d : primeFactors' (x `div` d) d
          | otherwise = primeFactors' x (d+1)

main = print $ maximum $ primeFactors 600851475143
