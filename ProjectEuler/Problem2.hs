fibs :: [Integer]
fibs = map fst $ iterate (\(a,b) -> (b,a+b)) (1,2)

main :: IO ()
main = do
  print . sum . filter even . takeWhile (<4000000) $ fibs
