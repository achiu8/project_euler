import Data.Char (digitToInt)

getNums :: IO [[Int]]
getNums = do
  contents <- readFile "p13.txt"
  return $ map intList $ words contents

intList :: String -> [Int]
intList = map digitToInt

sumNums :: [[Int]] -> [String]
sumNums xs = map place [0..49]
  where place 0 = show $ sumPlace 0 xs
        place i = show $ (sumPlace i xs) `mod` 10

sumPlace :: Int -> [[Int]] -> Int
sumPlace i xs
  | i >= length (xs !! 0) = 0
  | otherwise = sum (map (!! i) xs) + (sumPlace (i+1) xs) `div` 10

getNums' :: IO [Integer]
getNums' = do
  contents <- readFile "p13.txt"
  return $ map read $ words contents

main = do
  getNums >>= print . take 10 . concat . sumNums
  getNums' >>= print . take 10 . show . sum
