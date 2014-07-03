import Data.List
import Data.Maybe
import Data.Ratio

solve = denominator $ foldl1' (*) $ map (\(a,b) -> (read a) % (read b)) nums
  where
    nums = [ x | n <- [12..98], d <- [12..98]
           , n < d

           , let showN = show n
           , let n0 = showN !! 0
           , let n1 = showN !! 1
           , n1 /= '0'

           , let showD = show d
           , let d0 = showD !! 0
           , let d1 = showD !! 1
           , d1 /= '0'

           , let x' | n0 == d0 = (read $ return n1) % (read $ return d1)
                    | n0 == d1 = (read $ return n1) % (read $ return d0)
                    | n1 == d0 = (read $ return n0) % (read $ return d1)
                    | n1 == d1 = (read $ return n0) % (read $ return d0)
                    | otherwise = 0

           , x' /= 0
           , n % d == x'
           , let x = (showN,showD)
           ]

main :: IO ()
main = print solve
