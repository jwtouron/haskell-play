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

rotations :: Int -> [Int]
rotations n = map read $ head $ drop lenN $ map snd $ iterate step $ (cycle n',[])
  where n' = show n
        lenN = length n'
        step (a,b) = (tail a, take lenN a : b)

isCircularPrime :: Int -> Bool
isCircularPrime n = all isPrime $ rotations n

solve = length $ filter isCircularPrime $ filter isPrime [1..999999]

main = print solve
