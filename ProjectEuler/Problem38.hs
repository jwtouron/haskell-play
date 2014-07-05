import Data.List

isPandigital :: String -> Bool
isPandigital s = [1..length s] == (sort $ map (read . return) s)

solve :: Integer
solve = read $ head $ dropWhile (not . isPandigital) $ map (\x -> show x ++ show (x*2)) nums
  where nums = [9387,9386..9234] :: [Integer]

main :: IO ()
main = print solve
