import Control.Monad.State.Strict
import qualified Data.Map.Strict as M

primes :: [Integer]
primes = primes' $ 2 : [3,5..]
  where
    primes' (x:xs) = x : primes' [x' | x' <- xs, x' `mod` x /= 0]

primeFactors :: Integer -> [(Integer,Integer)]
primeFactors n = evalState primeFactor' (n,M.empty)
  where
    primeFactor' = do
      (n,m) <- get
      let smallestPFactor = smallestPrimeFactor n
      if smallestPFactor == n
        then return $ M.toList $ insertPrimeFactor smallestPFactor m
        else let n' = n `div` smallestPFactor
                 m' = insertPrimeFactor smallestPFactor m
             in do put (n',m')
                   primeFactor'
      where
        insertPrimeFactor pf m = M.insertWith (+) pf 1 m

smallestPrimeFactor :: Integer -> Integer
smallestPrimeFactor n = head $ dropWhile (\x -> x <= n && n `mod` x /= 0) primes

largestPrimeFactor :: Integer -> Integer
largestPrimeFactor = fst . last . primeFactors

main :: IO ()
main = do
  print $ largestPrimeFactor 600851475143
