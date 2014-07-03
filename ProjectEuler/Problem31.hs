--import qualified Data.Array.Unboxed as A
import qualified Data.HashMap.Strict as M

data GBCoin = OnePence | TwoPence | FivePence | TenPence
            | TwentyPence | FiftyPence | OnePound | TwoPound
            deriving (Eq,Ord,Show)

solve = length $ coinCombos
  where
    coinCombos = [ coins
                 | onePences <- map (flip replicate OnePence) [0..200]
                 , twoPences <- map (flip replicate TwoPence) [0..100]
                 , (coinTotal $ onePences ++ twoPences) <= 200
                 , fivePences <- map (flip replicate FivePence) [0..40]
                 , (coinTotal $ onePences ++ twoPences ++ fivePences) <= 200
                 , tenPences <- map (flip replicate TenPence) [0..20]
                 , (coinTotal $ onePences ++ twoPences ++ fivePences ++ tenPences) <= 200
                 , twentyPences <- map (flip replicate TwentyPence) [0..10]
                 , (coinTotal $ onePences ++ twoPences ++ fivePences ++ tenPences ++
                                twentyPences) <= 200
                 , fiftyPences <- map (flip replicate FiftyPence) [0..4]
                 , (coinTotal $ onePences ++ twoPences ++ fivePences ++ tenPences ++
                                twentyPences ++ fiftyPences) <= 200
                 , onePounds <- map (flip replicate OnePound) [0..2]
                 , (coinTotal $ onePences ++ twoPences ++ fivePences ++ tenPences ++
                                twentyPences ++ fiftyPences ++ onePounds) <= 200
                 , twoPounds <- map (flip replicate TwoPound) [0..1]
                 , let coins = onePences ++ twoPences ++ fivePences ++ tenPences ++ twentyPences ++
                               fiftyPences ++ onePounds ++ twoPounds
                 , coinTotal coins == 200]

coinTotal :: [GBCoin] -> Int
coinTotal = sum . map coinWorth

coinWorth :: GBCoin -> Int
coinWorth OnePence = 1
coinWorth TwoPence = 2
coinWorth FivePence = 5
coinWorth TenPence = 10
coinWorth TwentyPence = 20
coinWorth FiftyPence = 50
coinWorth OnePound = 100
coinWorth TwoPound = 200

main :: IO ()
main = print solve

-- much faster way

solveDynamic target coins = table
  where
    go 0 _ = 1
    go 1 _ = 1
    go t c = if c > t
               then table M.! ()
    table :: M.HashMap (Int,Int) Int
    table = M.fromList [((t,c), go t c) | t <- [0..target], c <- coins]

--solveDynamic :: Int -> [Int] -> _
-- solveDynamic target coins = (solTable,coinTable)
--   where
--     go 0 _ = 1
--     go 1 _ = 1
--     go t c = if c > t
--              then solTable A.! (c-1)
--              else solTable A.! (c-1)
--     solTable :: A.UArray (Int,Int) Int
--     solTable = A.listArray ((0,1), (target,length coins)) [go t c | t <- [0..target], c <- coins]
--     coinTable :: A.UArray Int Int
--     coinTable = A.listArray (1,length coins) coins
--     coinValue ci = coinTable A.! ci

dynamicSolution = solveDynamic 10 [1,2,5,10,20,50,100,200]
