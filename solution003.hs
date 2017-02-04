factors :: (Integral a) => a -> [a]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Integer -> Bool
prime n
    | len > 2   = False
    | otherwise = True
    where len = length $ factors n

solution003 :: Integer
solution003 = maximum [x | x <- factors 600851475143, prime x]