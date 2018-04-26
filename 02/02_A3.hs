{-
max_length [[1,2,3], [4,5,6], [7,8], [9], [1,2,3,4]] = 4
max_length [[], [], []] = 0
max_length [] = -1 (Sonderbehandlung auch anders möglich)
-}

length' :: [Int] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

max' :: Int -> Int -> Int
max' x y
  | x > y = x
  | otherwise = y
  
max_length :: [[Int]] -> Int
max_length [] = -1
max_length (x:xs) = max' (length' x) (max_length xs)
