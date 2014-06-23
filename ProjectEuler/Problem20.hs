factorial :: Integer -> Integer
factorial 1 = 1
factorial n = n * factorial (n - 1)

solve = sum . map (read . return) . show . factorial $ 100
