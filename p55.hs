isLychrel :: Integer -> Bool
isLychrel n = go (n + reversed n) 1
  where go n x
          | x >= 50 = True
          | isPalindrome n = False
          | otherwise = go (n + reversed n) (x+1)

reversed :: Integer -> Integer
reversed = read . reverse . show

isPalindrome :: Integer -> Bool
isPalindrome n = n == reversed n

main = print . length $ filter isLychrel [1..9999]
