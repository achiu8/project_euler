import Data.Char (isDigit, digitToInt)

digitList :: IO String
digitList = readFile "p08.txt" >>= return . filter isDigit

adjProduct :: String -> Int
adjProduct = product . map digitToInt

allProducts :: String -> [Int]
allProducts num@(x:xs)
  | length num < 13 = []
  | otherwise = adjProduct (take 13 num) : allProducts xs

main = digitList >>= print . maximum . allProducts
