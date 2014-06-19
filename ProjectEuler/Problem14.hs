import qualified Data.IntMap.Strict as M
import Control.Monad.State.Lazy
import Data.List

collatzStep :: Int -> Int
collatzStep n | n `mod` 2 == 0 = n `div` 2
              | otherwise = 3 * n + 1

collatz :: Int -> State (M.IntMap Int) Int
collatz 1 = return 1
collatz n = do
  m <- get
  case M.lookup n m of
    Just x -> return x
    Nothing -> do
      len <- collatz $ collatzStep n
      modify (M.insert n (len+1))
      return $ len + 1

solve = foldl' max 0 collatzChains
  where
    collatzChains = evalState (mapM collatz [1..999999]) M.empty

main = print solve
