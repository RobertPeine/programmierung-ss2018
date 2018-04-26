{-
Das Einfügen von Elementen am Anfang einer Liste ist mit einer Laufzeit
von O(1) sehr effizient. Das Einfügen am Ende besitzt hingegen eine
Laufzeit von O(n), wobei n die Länge der Liste ist.

Durch den Queue-Datentyp wird das schnelle Einfügen am Ende ermöglicht.
-}

type Queue = ([Int],[Int])

isEmpty :: Queue -> Bool
isEmpty ([],[]) = True
isEmpty _ = False

{-
Die Funktion queueify wird genutzt, um die Invariante
Queue (l, r) nicht leer <=> l nicht leer
zu realisieren
-}

queueify :: Queue -> Queue
queueify ([],rs) = (reverse rs, [])
queueify q = q

enqueue :: Int -> Queue -> Queue
enqueue i (ls, rs) = queueify (ls, i:rs)

first :: Queue -> Int
first ([],[]) = error "empty queue"
first (l:_,_) = l

rest :: Queue -> Queue
rest ([],[]) = error "empty queue"
rest (_:xs,ys) = queueify (xs,ys)
