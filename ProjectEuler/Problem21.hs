divisors :: Int -> [Int]
divisors n = [n' | n' <- [1..n`div`2], n `mod` n' == 0]

solve = sum [x | x <- [1..9999], let s = sum $ divisors x, x == (sum $ divisors s), s /= x]

main = print solve
