import Data.Word

isPrime :: Word64 -> Bool
isPrime n | n == 1 = False
          | n < 4 = True
          | n `mod` 2 == 0 = False
          | n < 9 = True
          | n `mod` 3 == 0 = False
          | otherwise = isPrime'
  where
    isPrime' = go 5
    r = floor $ sqrt $ fromIntegral n
    go f | f > r = True
         | n `mod` f == 0 = False
         | n `mod` (f+2) == 0 = False
         | otherwise = go (f+6)

primes :: [Word64]
primes = filter isPrime $ 2 : [3,5..]

solve = sum $ takeWhile (<2000000) primes

main = print solve
