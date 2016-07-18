import Data.List (nub)

power :: Integer -> Int -> Integer
power a b = product $ replicate b a

main = print . length $ nub [ power a b | a <- [2..100], b <- [2..100] ]
