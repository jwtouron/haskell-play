sumOfSquares n = sum $ map (^2) [1..n]

squareOfSum n = sum [1..n] ^ 2

solve :: Integer -> Integer
solve n = squareOfSum n - sumOfSquares n
