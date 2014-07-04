import Data.List

truncateFromLeft :: Int -> [Int]
truncateFromLeft = map read . init . tail . tails . show

truncateFromRight :: Int -> [Int]
truncateFromRight = map (read . reverse) . init . tail . tails . reverse . show

isPrime :: Int -> Bool
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

solve :: Int
solve = sum nums
  where
    nums = take 11 $ filter filterPrimeTruncatables primes
    primes = filter isPrime [11..]
    filterPrimeTruncatables x = (all isPrime $ truncateFromLeft x) && (all isPrime $ truncateFromRight x)

main :: IO ()
main = print solve
