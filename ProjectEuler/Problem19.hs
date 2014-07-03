data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec deriving (Eq,Enum)
data DayOfWeek = Sun | Mon | Tue | Wed | Thu | Fri | Sat deriving (Enum,Show,Eq)

gaussian :: Int -> Month -> Int -> DayOfWeek
gaussian day month year = toEnum w
  where
    m = fromIntegral $ ((fromEnum month + 10) `mod` 12) + 1
    d = day
    year' = if month == Jan || month == Feb then year - 1 else year
    y = read $ drop 2 $ show year'
    c = read $ take 2 $ show year'
    w = abs $ (d + floor (2.6 * m - 0.2) + y + (y `div` 4) + (c `div` 4) - (2 * c)) `mod` 7

solve = length [dow | month <- [Jan .. Dec], year <- [1901..2000], let dow = gaussian 1 month year, dow == Sun]
