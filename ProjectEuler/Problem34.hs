import qualified Data.Array.Unboxed as A
import qualified Data.HashMap.Strict as M

factorial :: Int -> Int
factorial 0 = 1
factorial n = go n 1
  where go 1 acc = acc
        go n' acc = go (n' - 1) (n' * acc)

solve :: Int -> Int
solve n = sum [x | x <- [3..n], (sum $ digitFactorials x) == x]
  where
    digitFactorials :: Int -> [Int]
    digitFactorials = map ((factorials A.!) . (digitMap M.!)) . show
    factorials :: A.UArray Int Int
    factorials = A.listArray (0,9) $ map factorial [0..9]
    digitMap :: M.HashMap Char Int
    digitMap = M.fromList [(c, read [c]) | c <- ['0'..'9']]

main :: IO ()
main = print $ solve 2540160
