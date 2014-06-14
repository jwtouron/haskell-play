-- Demonstrates the hackage foldl package
-- URL: http://hackage.haskell.org/package/foldl

import qualified Control.Foldl as L
import Control.Applicative

data ListInfo = ListInfo { liHead :: Maybe Int
                         , liAvg :: Double
                         , liMin :: Maybe Int
                         , liMax :: Maybe Int
                         , liAnyEven :: Bool
                         , liAllEven :: Bool
                         , liLast :: Maybe Int
                         } deriving Show

main :: IO ()
main = do
  let listInfo = L.fold (ListInfo <$> L.head <*> average <*> L.minimum <*> L.maximum <*> L.any even <*> L.all even <*> L.last) [1..1000000::Int]
  print listInfo
  where
    average :: L.Fold Int Double
    average = ((/).fromIntegral) <$> L.sum <*> L.genericLength
