toBin :: Int -> [Int]
toBin 0 = []
toBin n = let (q,r) = n `divMod` 2 in toBin q ++ [r]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs

valid :: Int -> Bool
valid n = isPalindrome (show n) && isPalindrome (toBin n)

main = print . sum $ filter valid [1..999999]
