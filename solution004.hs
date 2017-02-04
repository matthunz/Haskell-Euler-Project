digits :: Integer -> [Integer]
digits n = map (\x -> read [x] :: Integer) (show n)

palindrome :: [Integer] -> Bool
palindrome [] = True
palindrome [x] = True
palindrome xs = head xs == last xs && palindrome (init (tail xs))

joiner :: [Integer] -> Integer
joiner = read . concatMap show

solution004 :: Integer
solution004 = maximum $ map joiner (filter palindrome $ map digits products)
    where products = concat $ map (\x -> map (*x) [100..999]) [100..999]