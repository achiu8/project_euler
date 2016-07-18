import Data.List (sort)

valid :: Int -> Bool
valid n = all same [1..6]
  where same i = digits (n*i) == digits n
        digits = sort . show

main = print . head $ dropWhile (not . valid) [1..]
