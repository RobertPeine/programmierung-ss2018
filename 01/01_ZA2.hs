{-
Es gibt genau einen Binärbaum mit einem Knoten. Ein Binärbaum mit n + 1
Knoten besteht aus einem Wurzelknoten und zwei Teilbäumen, deren
Knotensumme auf n = m + (n - 1 - m) kommt, wobei m die Größe des linken
Teilbaums ist. Mit der Funktion go summieren wir über alle Möglichkeiten
für m.
-}

countBinTrees :: Int -> Int
countBinTrees 0 = 0
countBinTrees 1 = 1
countBinTrees n = go (n-1)
  where
    go 0 = 0
    go m = go (m-1) + countBinTrees (n-1-m) * countBinTrees m

{-
Die alternative Lösung verwendet die Catalan-Zahlen - eine Folge aus der
Kombinatorik für die Anzahl von Binärbäumen, die vollständigen
Klammerungen von Termen etc., welche durch eine geschlossene Formel
berechnet werden können.
-}

countBinTrees' :: Int -> Int
countBinTrees' n
  | even n = 0
  | otherwise =
      let m = div (n-1) 2
      in div (fac (2*m)) (fac m * fac (m+1))

-- div (n-1) 2 <=> (n-1) `div` 2
