fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib i = fib (i-1) + fib (i-2)
