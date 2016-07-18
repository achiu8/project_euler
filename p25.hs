fib :: Int -> Integer
fib 1 = 1
fib 2 = 1
fib 3 = 2
fib n = fib' n [2,1,1]
  where fib' n fibs@(x:y:ys)
          | length fibs == n-1 = x + y
          | otherwise = fib' n (x+y : fibs)

digits :: Integer -> Int
digits = length . show

main = print $ snd . head . dropWhile (\(fib,i) -> digits fib < 1000) $ map (\i -> (fib i,i)) [1..]
