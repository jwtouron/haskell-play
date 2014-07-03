import Control.Applicative
import Data.List
import Data.List.Split

wordWorth :: String -> Int
wordWorth = sum . map (\c -> fromEnum c - 64)

solve names = foldl' (\s (pos,word) -> s + pos * wordWorth word) 0 $ zip [1..] $ sort names

main :: IO ()
main = do
  names <- map (filter (/= '"')) . wordsBy (==',') <$> readFile "names.txt"
  print $ solve names
