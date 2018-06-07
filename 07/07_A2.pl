edge(1, 1).
edge(1, 4).
edge(1, 2).
edge(3, 2).
edge(4, 3).
path(U, U).
path(U, W) :- edge(U, V), path(V, W).
