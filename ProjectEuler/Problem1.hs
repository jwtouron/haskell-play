multipleOf x y = y `mod` x == 0
multipleOf3 = multipleOf 3
multipleOf5 = multipleOf 5
multipleOf3Or5 n = multipleOf3 n || multipleOf5 n

main = do
  print $ sum $ filter multipleOf3Or5 [1..999]
