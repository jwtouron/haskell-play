import qualified Data.HashSet as S

solve :: Int
solve = S.size $ set
  where
    set :: S.HashSet Integer
    set = S.fromList [a^b | a <- [2..100], b <- [2..100]]
