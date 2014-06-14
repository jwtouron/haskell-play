-- Demonstrates a method of memoization in Haskell using arrays as
-- lookup tables, relying on lazy evaluation
-- See:
--     http://en.wikipedia.org/wiki/Dynamic_programming#Egg_dropping_puzzle
--     http://www.geeksforgeeks.org/dynamic-programming-set-11-egg-dropping-puzzle/
--     http://jelv.is/blog/Lazy-Dynamic-Programming/

import qualified Data.Array.IArray as A

-- n is number of eggs, k is number of stories
eggDrop :: Int -> Int -> Int
eggDrop n k = go n k
  where
    go _ 0 = 0
    go _ 1 = 1
    go 1 k = k
    go n k = 1 + minimum [max (table A.! ((n-1), (x-1))) (table A.! (n, (k-x))) | x <- [1..k]]
    table = A.listArray bounds [go n k | (n, k) <- A.range bounds] :: A.Array (Int, Int) Int
    bounds = ((0, 0), (n, k))

main :: IO ()
main = do
  print $ eggDrop 2 36
  print $ eggDrop 5 100
