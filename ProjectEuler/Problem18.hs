import Control.Monad.State.Strict
import qualified Data.HashMap.Strict as M
import Data.List

triangleMapFromString :: String -> M.HashMap (Int,Int) Int
triangleMapFromString = insertRow . zip [1..] . map (zip [1..] . map read . words) . lines
  where
    insertRow :: [(Int,[(Int,Int)])] -> M.HashMap (Int,Int) Int
    insertRow = foldl' insertNum M.empty
    insertNum tMap (rowIdx,row) = foldl' (\tMap (colIdx,num) -> M.insert (rowIdx,colIdx) num tMap) tMap row

triangle = triangleMapFromString
  "75\n\
  \95 64\n\
  \17 47 82\n\
  \18 35 87 10\n\
  \20 04 82 47 65\n\
  \19 01 23 75 03 34\n\
  \88 02 77 73 07 63 67\n\
  \99 65 04 28 06 16 70 92\n\
  \41 41 26 56 83 40 80 70 33\n\
  \41 48 72 33 47 32 37 16 94 29\n\
  \53 71 44 65 25 43 91 52 97 51 14\n\
  \70 11 33 28 77 73 17 78 39 68 17 57\n\
  \91 71 52 38 17 14 91 43 58 50 27 29 48\n\
  \63 66 04 68 89 53 67 30 73 16 69 87 40 31\n\
  \04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"

maxTriangleRow tMap = fst $ maximum $ M.keys tMap

solve tMap = evalState (go $ maxTriangleRow tMap - 1) tMap M.! (1,1)
  where
    go 0 = get
    go n = do
      tMap <- get
      let tMap' = foldl' updateNum tMap [1..n]
      put tMap'
      go $ n - 1

      where
        updateNum tMap n' = let l = tMap M.! (n+1,n')
                                r = tMap M.! (n+1,n'+1)
                                c = tMap M.! (n,n')
                            in M.insert (n,n') (c + max l r) tMap

main = print $ solve triangle
