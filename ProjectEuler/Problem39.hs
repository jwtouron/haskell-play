import qualified Data.HashMap.Strict as M

solve :: Int -> Int
solve n = fst $ M.foldrWithKey findMaximized (0,0) $ M.fromListWith (+) nums
  where
    nums :: [(Int,Int)]
    nums = [(a+b+c,1) | c <- [1..n-2], b <- [1..c], a <- [1..b], a + b + c <= n,
            (a^(2::Int)) + (b^(2::Int)) == (c^(2::Int))]
    findMaximized k v (n,m) = if v > m then (k,v) else (n,m)

main :: IO ()
main = print $ solve 1000
