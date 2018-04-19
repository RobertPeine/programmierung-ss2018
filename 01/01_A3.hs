prod :: [Int] -> Int
prod [] = 1
prod (x:xs) = x * prod xs

-- x ist in diesem Fall das erste Element der Liste und xs die Restliste

------------------------------------------------------------------------

rev :: [Int] -> [Int]
rev [] = []
rev (x:xs) = rev xs ++ [x]

-- mit ++ können Listen zusammengeführt werden

------------------------------------------------------------------------

-- rem ist bereits definiert
-- Ausblenden möglich mit: import Prelude hiding (rem)

rem' :: Int -> [Int] -> [Int]
rem' _ [] = []
rem' n (x:xs)
  | x /= n = x : rem' n xs -- ebenso möglich: x /= n = [x] ++ rem' n xs
  | otherwise = rem' n xs
  
-- /= wird verwendet, um auf Ungleichheit zu prüfen
-- Es gilt: not (x == n) <=> x /= n
  
-- oder:

rem'' :: Int -> [Int] -> [Int]
rem'' _ [] = []
rem'' n (x:xs)
  | x == n = rem'' n xs
  | otherwise = x : rem'' n xs

------------------------------------------------------------------------

isOrd :: [Int] -> Bool
isOrd [] = True
isOrd [x] = True -- ebenso möglich: (x:[]) = True
isOrd (x:y:xs)
  | x <= y = isOrd (y:xs)
  | otherwise = False

-- oder etwas kürzer:

isOrd' :: [Int] -> Bool
isOrd' [] = True
isOrd' [x] = True
isOrd' (x:y:xs) = x <= y && isOrd' (y:xs)

------------------------------------------------------------------------

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- oder:

merge' :: [Int] -> [Int] -> [Int]
merge' [] ys = ys
merge' xs [] = xs
merge' xxs@(x:xs) yys@(y:ys)
  | x < y = x : merge' xs yys
  | otherwise = y : merge' xxs ys
