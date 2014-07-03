import Control.Monad.State.Strict
import qualified Data.HashMap.Strict as M
import Data.List

data Direction = DLeft | DRight | DUp | DDown deriving (Eq,Show)

spiral :: Int -> M.HashMap (Int,Int) Int
spiral n | even n = error "spiral side must be odd"
         | otherwise = evalState go (start, start, 1, M.singleton (start,start) 1, DRight)
  where
    start = (n `div` 2) + 1
    go = do
      (row,col,x,spiralMap,dir) <- get

      if x == n * n
        then return spiralMap
        else do
          let (row',col') = case dir of
                DLeft -> (row,col-1)
                DRight -> (row,col+1)
                DUp -> (row-1,col)
                DDown -> (row+1,col)

          let x' = x + 1

          let spiralMap' = M.insert (row',col') x' spiralMap

          let dir' | (dir == DLeft) && (not $ M.member (row'-1,col') spiralMap') = DUp
                   | (dir == DRight) && (not $ M.member (row'+1,col') spiralMap') = DDown
                   | (dir == DUp) && (not $ M.member (row',col'+1) spiralMap') = DRight
                   | (dir == DDown) && (not $ M.member (row',col'-1) spiralMap') = DLeft
                   | otherwise = dir

          row' `seq` col' `seq` x' `seq` spiralMap' `seq` dir' `seq`
            put (row',col',x',spiralMap',dir')

          go

forwardDiagonalSum :: M.HashMap (Int,Int) Int -> Int -> Integer
forwardDiagonalSum sp n = foldl' (\s x -> s + (fromIntegral $ sp M.! (x,x))) 0 [1..n]

backwardDiagonalSum :: M.HashMap (Int,Int) Int -> Int -> Integer
backwardDiagonalSum sp n = foldl' (\s x -> s + (fromIntegral $ sp M.! (x,n-x+1))) 0 [1..n]

diagonalSum :: M.HashMap (Int,Int) Int -> Int -> Integer
diagonalSum sp n = forwardDiagonalSum sp n + backwardDiagonalSum sp n - 1

main :: IO ()
main = print $ diagonalSum (spiral 1001) 1001
