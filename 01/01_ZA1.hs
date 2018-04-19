fibs :: [Int]
fibs = fibs' 0
  where
    fibs' x = fib x : fibs' (x+1)

-- take 7 fibs liefert die ersten 7 Fibonacci-Zahlen
