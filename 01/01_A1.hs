fac :: Int -> Int
fac 0 = 1
fac n = n * fac (n-1)

{-
fac 3
= 3 * fac 2
= 3 * (2 * fac 1)
= 3 * 2 * (1 * fac 0)
= 3 * 2 * 1 * 1
= 6
-}

------------------------------------------------------------------------

sumFacs :: Int -> Int -> Int
sumFacs n m
  | m < n = 0
  | otherwise = fac n + sumFacs (n+1) m

-- oder:

sumFacs' :: Int -> Int -> Int
sumFacs' n m
  | n <= m = fac n + sumFacs' (n+1) m
  | otherwise = 0
  
-- oder:

sumFacs'' :: Int -> Int -> Int
sumFacs'' n m = 
  if n <= m
  then fac n + sumFacs'' (n+1) m
  else 0
