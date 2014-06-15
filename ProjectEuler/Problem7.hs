-- simple method
primes :: [Integer]
primes = primes' $ 2 : [3,5..]
  where
    primes' (x:xs) = x : primes' [x' | x' <- xs, x' `mod` x /= 0]
solve = primes !! 10000

main = print $ filter isPrime [1..] !! 10000

-- based off the PE 7 overview, much quicker than above method (10ms, vs 6 sec)
isPrime :: Integer -> Bool
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
