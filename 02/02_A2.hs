import Prelude hiding (words, unwords)

unwords :: [String] -> String
unwords [] = ""
unwords [x] = x -- ebenso möglich: (x:[]) = x
unwords (x:xs) = x  ++ ' ' : unwords xs
-- oder: unwords (x:xs) = x  ++ " " ++ unwords xs

------------------------------------------------------------------------

words :: String -> [String]
words s = f "" s
  where
    f :: String -> String -> [String]
    f ys "" = [ys]
    f ys (x:xs)
      | x /= ' ' = f (ys ++ [x]) xs
      | ys == "" = f "" xs
      | otherwise = ys : f "" xs

-- oder:

words' :: String -> [String]
words' [] = []
words' (' ':cs) = words' cs
words' cs = let (w, cs') = takeWord cs in w : words' cs'
  where
    takeWord [] = ([],[])
    takeWord (' ':cs) = ([], cs)
    takeWord (c:cs) = let (w, cs') = takeWord cs in (c:w, cs')
