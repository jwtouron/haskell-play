import qualified Data.HashSet as S

divisors :: Int -> [Int]
divisors n = [n' | n' <- [1..n`div`2], n `mod` n' == 0]

isAbundant :: Int -> Bool
isAbundant n = (sum $ divisors n) > n

abundantNums :: [Int]
abundantNums = filter isAbundant [12..28123]

solve :: Int
solve = S.foldl' (+) 0 $ allNums `S.difference` sumOf2Abundants
  where
    sumOf2Abundants = S.fromList [x + y | x <- abundantNums, y <- abundantNums, x <= y, x + y <= 28123]
    allNums = S.fromList [1..28123]

main = print solve
