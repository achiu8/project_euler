sumsquare :: Int -> Int
sumsquare n = foldl1 (\sum x -> sum + x*x) [1..n]

squaresum :: Int -> Int
squaresum n = let x = sum [1..n] in x*x

sumsquarediff :: Int -> Int
sumsquarediff n = squaresum n - sumsquare n

main = print $ sumsquarediff 100
