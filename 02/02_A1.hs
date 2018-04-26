pack :: [Char] -> [[Char]]
pack [] = []
pack (x:xs) = pack' x (x:xs) : pack'' x xs

pack' :: Char -> [Char] -> [Char]
pack' _ []= []
pack' y (x:xs)
  | y == x = x : pack' y xs
  | otherwise = []

pack'' :: Char -> [Char] -> [[Char]]
pack'' _ [] = []
pack'' y (x:xs)
  | y == x = pack'' y xs
  | otherwise = pack (x:xs)

-- oder:

pack''' [] = []
pack''' (x:xs) = ys : pack''' zs
  where
    (ys, zs) = takeall x (x:xs)
    takeall _ [] = ([], [])
    takeall x (y:ys)
      | x == y = let (us, vs) = takeall x ys in (y:us, vs)
      | otherwise = ([], (y:ys))

------------------------------------------------------------------------

encode :: [Char] -> [(Int, Char)]
encode xs = f (pack xs)
  where
    f [] = []
    f ((x:xs):ys) = (length (x:xs), x) : f ys
    -- oder: f (y@(x:xs):ys) = (length y, x) : f ys
    -- oder: f (y:ys) = (length y, head y) : f ys

------------------------------------------------------------------------

decode :: [(Int, Char)] -> [Char]
decode [] = []
decode ((n, x):xs) = repeat n x ++ decode xs
  where
    repeat 0 _ = []
    repeat n x = x : repeat (n-1) x

------------------------------------------------------------------------

rotate :: [Int] -> Int -> [Int]
rotate [] _ = []
rotate xxs@(x:xs) n
  | n == 0 = xxs
  | n > 0 = rotate (xs ++ [x]) (n-1)
  | otherwise = rotate (last xxs : take (length xxs - 1) xxs) (n+1)

-- oder:

rotate' :: [Int] -> Int -> [Int]
rotate' [] _ = []
rotate' xxs@(x:xs) n
  | n == 0 = xxs
  | n > 0 = rotate' (xs ++ [x]) (n-1)
  | otherwise = rotate' xxs (length xxs + n)

-- Verwendung von n `mod` length xxs zur Reduzierung der Durchläufe:

rotate'' :: [Int] -> Int -> [Int]
rotate'' [] _ = []
rotate'' xxs@(x:xs) n
  | n `mod` length xxs == 0 = xxs
  | n > 0 = rotate'' (xs ++ [x]) (n `mod` length xxs - 1)
  | otherwise = rotate'' xxs (length xxs + n `mod` length xxs)

-- Es gilt: mod n (length xxs) <=> n `mod` length xxs
