import Data.List (find)

triangle :: Int -> Int
triangle n = n * (n+1) `div` 2

pentagonal :: Int -> Int
pentagonal n = n * (3*n - 1) `div` 2

hexagonal :: Int -> Int
hexagonal n = n * (2*n - 1)

valid :: Int -> Bool
valid n = n `elem` pentas && n `elem` hexas
  where pentas = map pentagonal [165..]
        hexas = map hexagonal [143..]

main = do
  let triangles = map triangle [286..]
  print $ find valid triangles
