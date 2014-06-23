fibs :: [Integer]
fibs  = map fst . iterate (\(a,b) -> (b,a+b)) $ (0,1)

solve n = head $  dropWhile ((<n) . length . show . snd) $ zip [0..] fibs

main = print $ solve 1000
