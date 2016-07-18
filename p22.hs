import Data.List (sort, elemIndex)
import Data.List.Split

getNames :: IO [String]
getNames = do
  contents <- readFile "p22.txt"
  return $ sort . map (filter (/= '"')) $ splitOn "," contents

value :: String -> Int -> Int
value s i = i * (sum $ map letterValue s)

letterValue :: Char -> Int
letterValue c = x + 1
  where Just x = elemIndex c ['A'..'Z']

main = do
  names <- getNames
  print $ sum $ map (\i -> value (names !! (i-1)) i) [1..length names]
