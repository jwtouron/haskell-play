import Data.List

isPandigital :: String -> Bool
isPandigital s = [1..length s] == (sort $ map (read . return) s)

solve :: Integer
solve = solve' [1..9] [1234..9876] + solve' [12..98] [123..987]
  where
    solve' xs ys = sum $ nub [ z | x <- xs, y <- ys
                             , x /= y
                             , let z = x * y
                             , let showZ = show z
                             , length showZ == 4
                             , let showX = show x
                             , let showY = show y
                             , length showX + length showY + length showZ == 9
                             , let showXYZ = showX ++ showY ++ showZ
                             , isPandigital showXYZ]

main :: IO ()
main = print solve
