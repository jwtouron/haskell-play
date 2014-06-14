-- Demonstrating use of the State Monad to calculate fibonacci numbers

import System.Environment
import Control.Monad.State.Strict
import qualified Data.HashMap.Strict as M

fib' :: Integer -> State (M.HashMap Integer Integer) Integer
fib' n
   | n == 0 = return 0
   | n == 1 = return 1
   | otherwise = do
     m <- get
     if M.member n m
       then return $ (m M.! n)
       else do
         x1 <- fib' (n-1)
         x2 <- fib' (n-2)
         modify (M.insert n (x1+x2))
         return $ x1 + x2

fib :: Integer -> Integer
fib n = evalState (fib' n) M.empty

fibs :: Integer -> [Integer]
fibs n = evalState (mapM fib' [0..n-1]) M.empty
