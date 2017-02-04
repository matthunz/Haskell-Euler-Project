fib :: (Integral a) => a -> a
fib 0 = 1
fib 1 = 1
fib x = fib (x - 2) + fib (x - 1)

solution002 :: Integer
solution002 = sum $ filter even $ takeWhile (< floor 4e6 ) $ map fib [1..]