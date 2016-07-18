import Data.Char (digitToInt)

valid :: Int -> Bool
valid n = n == sumDigitFifthPowers n

sumDigitFifthPowers :: Int -> Int
sumDigitFifthPowers = sum . map fifthPower . show
  where fifthPower = product . replicate 5 . digitToInt

main = do
  let max = sumDigitFifthPowers 9999
  print . sum $ filter valid [2..max]
