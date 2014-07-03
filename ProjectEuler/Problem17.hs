import qualified Data.Array.Unboxed as A
import Data.Char

ones :: A.Array Int String
ones = A.listArray (0,9) ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

tens :: A.Array Int String
tens = A.listArray (1,9) ["ten", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

teens :: A.Array Int String
teens = A.listArray (11,19) ["eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen",
                             "seventeen", "eighteen", "nineteen"]

oneThousand :: String
oneThousand = "one thousand"

writeOut :: Int -> String
writeOut n | length showN == 1 = ones A.! n
           | length showN == 2 = writeOutDouble showN
           | length showN == 3 = writeOutTriple showN
           | n == 1000 = oneThousand
  where
    showN = show n
    writeOutDouble num@(a:b:[]) | a == '0' = ones A.! (read $ return b)
                                | b == '0' = tens A.! (read $ return a)
                                | a == '1' = teens A.! read num
                                | otherwise = tens A.! (read $ return a) ++ "-" ++ ones A.! (read $ return b)
    writeOutTriple (a:b:c:[]) = ones A.! (read $ return a) ++ " hundred" ++
                                  if b /= '0' || c /= '0'
                                    then " and " ++ writeOutDouble (b:c:[])
                                    else ""

solve m n = sum $ map (length . filter isAlpha . writeOut) [m..n]
