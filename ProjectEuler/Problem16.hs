main :: IO ()
main = print $ sum . map (read . return) . show $ 2^1000
