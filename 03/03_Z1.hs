import Prelude hiding (foldl)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs

-- Für Fortgeschrittene:

foldl' f a xs = foldr (\x g -> flip f x . g) id xs a
