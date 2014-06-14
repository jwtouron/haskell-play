isPalindrome n = n == reverse n

main = do
  print $ maximum [n | x <- [100..999], y <- [100..999], let n = x*y, isPalindrome $ show n]
