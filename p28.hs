upright :: Int -> Int
upright n = sum [ x*x | x <- [1,3..n] ] - 1

downright :: Int -> Int
downright n = upright n - mults n 6

downleft :: Int -> Int
downleft n = upright n - mults n 4

upleft :: Int -> Int
upleft n = upright n - mults n 2

mults n x = sum $ map (*x) [1..n `div` 2]

diagSum :: Int -> Int
diagSum n = upright n + downright n + downleft n + upleft n + 1

main = print $ diagSum 1001
