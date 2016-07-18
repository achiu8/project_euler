import Data.Char (digitToInt)

factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial 2 = 2
factorial 3 = 6
factorial 4 = 24
factorial 5 = 120
factorial 6 = 720
factorial 7 = 5040
factorial 8 = 40320
factorial 9 = 362880

factsum :: Int -> Int
factsum n = foldl parseFact 0 (show n)
  where parseFact acc x = acc + factorial (digitToInt x)

valid :: Int -> Bool
valid n = n == factsum n

main = print . sum $ filter valid [3..1000000]
