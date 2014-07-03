import Data.List

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
solve = maxA * maxB
  where
    (maxA,maxB,_) = maximumBy (\(_,_,n1) (_,_,n2) -> n1 `compare` n2) xs
    xs = [(a,b,n) | a <- [(-999)..999], b <- [(-999)..999],
          let n = (head $ dropWhile (\x -> isPrime $ abs $ x*x + a*x + b) [0..]) - 1]

main :: IO ()
main = print solve
