fib :: Int -> Int
fib 1 = 1
fib 2 = 2
fib n = fib' n [2,1]
  where fib' n fibs@(x:y:ys)
          | length fibs == n-1 = x+y
          | otherwise = fib' n (x+y : fibs)

addifeven :: Int -> Int -> Int
addifeven sum x = if even x then sum + x else sum

main = print $ foldl addifeven 0 . takeWhile (<=4000000) $ map fib [1..]
