primeSummation :: Int -> Int
primeSummation = sum . primes

primes :: Int -> [Int]
primes n = 2 : sieve [3,5..n]
  where sieve [] = []
        sieve (x:xs) = let filtered = filter (\n -> n `mod` x /= 0) xs
                       in x : sieve filtered
        
main = print $ primeSummation 100
