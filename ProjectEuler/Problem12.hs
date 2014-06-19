import Control.Monad.State.Strict
import Data.Word
import Data.List

numDivisors :: Word64 -> Word64
numDivisors 0 = 0
numDivisors n = let numDivs = foldl' (\nod i -> if n `mod` i == 0 then nod+2 else nod) 0 [1..sqrtN]
                in if n == sqrtN * sqrtN
                     then numDivs - 1
                     else numDivs
  where sqrtN = floor $ sqrt $ fromIntegral n

solve = evalState go (0,1)
  where
    go = do
      (number,i) <- get
      if numDivisors number < 500
        then do
          put (number + i, i + 1)
          go
        else return number

main = print solve
