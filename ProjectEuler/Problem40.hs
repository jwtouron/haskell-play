import qualified Data.Array.Unboxed as A
import           Data.Char

solve :: Int
solve = product $ map ((digits A.!)) [1, 10, 100, 1000, 10000, 100000, 1000000]
  where
    digits :: A.UArray Int Int
    digits = A.listArray (1,1000000) $ map ((subtract 48) . ord) $ concatMap (show) [1::Int ..]

main :: IO ()
main = print solve
