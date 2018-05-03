data Tree a = Branch a (Tree a) (Tree a) | Leaf a deriving Show

testTree :: Tree Int
testTree =
  Branch 1 
    (Branch 2
      (Leaf 3)
      (Leaf 4)
    )
    (Branch 5
      (Leaf 6)
      (Branch 7
        (Leaf 8)
        (Leaf 9)
      )
    )

------------------------------------------------------------------------

depth :: Tree a -> Int
depth (Leaf _) = 1
depth (Branch _ l r) = min (depth l) (depth r) + 1

------------------------------------------------------------------------

paths :: Tree a -> Tree [a]
paths t = paths' [] t
  where
    paths' p (Leaf a) = Leaf (p ++ [a])
    paths' p (Branch a l r) =
      let p' = p ++ [a]
      in Branch p' (paths' p' l) (paths' p' r)

------------------------------------------------------------------------

tmap :: (a -> b) -> Tree a -> Tree b
tmap f (Leaf v) = Leaf (f v)
tmap f (Branch v l r) = Branch (f v) (tmap f l) (tmap f r)
