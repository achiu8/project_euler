import Data.List.Split
import Data.List (elemIndex)

loadWords :: IO [String]
loadWords = do
  all <- readFile "p042_words.txt"
  let quoted = splitOn "," all
  return $ map (filter (/= '"')) quoted

tnum :: Int -> Int
tnum n = (n * (n+1)) `div` 2

tnumUpTo :: Int -> [Int]
tnumUpTo n = takeWhile (<=n) $ map tnum [1..]

wordValue :: String -> Int
wordValue word = wordValue' word 0
  where wordValue' [] total = total
        wordValue' (x:xs) total = wordValue' xs (total + letterValue x)

letterValue :: Char -> Int
letterValue c = x + 1
  where Just x = elemIndex c ['A'..'Z']

main = do
  words <- loadWords
  let wordValues = map wordValue words
  let tnums = tnumUpTo $ maximum wordValues
  print . length $ filter (flip elem tnums) wordValues
