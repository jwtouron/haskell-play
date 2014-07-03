solve :: Integer -> Integer
solve p = sum $ map fst $ filter (\(a,b) -> a == b) $  map (\n -> (n,sumOfPows n p)) [2..355000]

sumOfPows :: Integer -> Integer -> Integer
sumOfPows n p = sum . map ((^p) . read . return) . show $ n

main :: IO ()
main = print $ solve 5
