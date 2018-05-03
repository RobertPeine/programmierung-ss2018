f :: [Int] -> Int
f xs = foldr product 1 (map square (filter even' xs))
  where
    even' x = mod x 2 == 0
    square x = x * x
    product x y = x * y

-- Haskell-Funktionen nutzen:

f' :: [Int] -> Int
f' xs = foldr (*) 1 (map (^2) (filter even xs))

-- Funktionskomposition nutzen:

f'' :: [Int] -> Int
f'' = foldr (*) 1 . map (^2) . filter even
