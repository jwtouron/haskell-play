pythagoreanTriplets :: Integer -> [(Integer,Integer,Integer)]
pythagoreanTriplets limit =
  [(a,b,c) | a <- [1..limit], b <- [a+1..limit], c <- [b+1..limit], a^2 + b^2 == c^2]

solve = let (a,b,c) = head $ filter (\(a,b,c) -> a + b + c == 1000) $ pythagoreanTriplets 500
        in a * b * c

main = print solve
