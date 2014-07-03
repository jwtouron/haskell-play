import qualified Data.HashMap.Lazy as LM
import qualified Data.IntMap as IM
import           Data.List (sort)

data GBCoin = OnePence | TwoPence | FivePence | TenPence
            | TwentyPence | FiftyPence | OnePound | TwoPound
            deriving (Eq,Ord,Show)

solve :: Int
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

-- much faster way
-- based on http://users.softlab.ntua.gr/~ttsiod/euler31.html

-- Thinking the same way:

-- Target in cents	only 1p	<= 2p	<= 5p	<= 10p	<= 20p	<= 50p	<= 100p	<= 200p
-- 1	1	1	1	1	1	1	1	1
-- 2	1	2	2	2	2	2	2	2
-- 3	1	2	2	2	2	2	2	2
-- 4	1	3	3	3	3	3	3	3
-- 5	1	3	4	4	4	4	4	4

-- 3 cents can be formed as (1p+1p+1p,1p+2p), so starting from column "<=2p", the number is 2
-- 4 cents can be formed as (4x1p, 2x1p+1x2p, 2x2p), so starting from column "<=2p", the number is 3
-- 5 cents can be formed as (5x1p, 3x1p+1x2p, 1x1p+2x2p, 1x5p), so starting from column "<=5p", the number is 4
-- etc...

-- So, now that we've done this process manually, what do we notice?
-- We notice that when forming the values in a line, the first column is always 1. There is only one way to form any target N, if you just use 1p coins: Nx1p.
-- We also notice that when filling up a cell, we check to see if the corresponding coin "fits" in. If it doesn't, we just copy the value of the cell on the left - i.e. the number of ways to form a target sum doesn't change, since we can't use this column's coin.
-- If the coin *does* fit, however, we then form a number by adding two things: (a) the number of ways we can form the target WITHOUT using the coin (which is on the cell on our left) plus (b) the number of ways we can form the target-columnCoin, i.e. the number of ways to form the remainder, if we subtract (i.e. use) the coin from our target.

-- For example, on line 5 (target: 5 cents) the highlighted cell of column "<=2p" is formed as 1+2:
-- There is 1 way to form a target of 5 cents using only 1p coins (the cell on the left has value 1)
-- If we use a coin of 2p, then the remainder is target-2p=5p-2p=3p, which we can lookup above, on line 3, and see that there are 2 ways to reach it, using "<=2p".

-- This lookup is key - we basically reuse previous calculations with a single lookup.

solveDynamic :: [Int] -> Int -> Int
solveDynamic coins target = go target $ maximum coins
  where
    -- go => max coin value available, target value
    go _ 1 = 1
    go 1 _ = 1
    go t c = if c > t
             then table LM.! (t, prevCoin IM.! c)
             else table LM.! (t, prevCoin IM.! c) + table LM.! (t-c,c)
    table = LM.fromList [((t,c), go t c) | t <- [0..target], c <- coins]
    prevCoin = IM.fromList $ let coins' = reverse $ sort coins in zip coins' $ tail coins'

main :: IO ()
main = print $ solveDynamic [1,2,5,10,20,50,100,200] 200
