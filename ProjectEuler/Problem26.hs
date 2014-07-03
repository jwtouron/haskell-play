isPrime :: Integer -> Bool
isPrime n | n == 1 = False
          | n < 4 = True
          | n `mod` 2 == 0 = False
          | n < 9 = True
          | n `mod` 3 == 0 = False
          | otherwise = isPrime'
  where
    isPrime' = go 5
    r = floor $ (sqrt $ fromIntegral n :: Double)
    go f | f > r = True
         | n `mod` f == 0 = False
         | n `mod` (f+2) == 0 = False
         | otherwise = go (f+6)

longestRecurringCycle :: Integer -> Integer
longestRecurringCycle d
  | (not $ isPrime d) = 0
  | otherwise = if null ns then 0 else head ns
  where
    ns = [n | n <- [1..d-1], (mod (10^n - 1) d) == 0]

solve :: Integer -> Integer
solve d = maximum $ map longestRecurringCycle [1..d]

main :: IO ()
main = print $ solve 999
