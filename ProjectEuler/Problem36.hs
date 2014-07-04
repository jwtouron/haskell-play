import Data.Char
import Numeric

isPalindrome :: String -> Bool
isPalindrome s = s == reverse s

showIntInBinary :: Int -> String
showIntInBinary n = showIntAtBase 2 intToDigit n ""

solve :: Int
solve = sum nums
  where
    nums = [x | x <- [1..999999], isPalindrome $ show x, isPalindrome $ showIntInBinary x]

main :: IO ()
main = print solve
