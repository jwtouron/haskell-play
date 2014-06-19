import qualified Data.Array.Unboxed as A

pathLength :: (Int,Int) -> Int
pathLength (r,c) = go (r,c)
  where
    go (0,0) = 0
    go (0,_) = 1
    go (_,0) = 1
    go (r',c') = lengthTable A.! (r'-1,c') + lengthTable A.! (r',c'-1)
    lengthTable :: A.Array (Int,Int) Int
    lengthTable = A.array ((0,0),(r,c)) [((r',c'), go (r',c')) | r' <- [0..r], c' <- [0..c]]

main :: IO ()
main = print $ pathLength (20,20)
