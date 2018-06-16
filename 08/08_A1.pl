nat(0).
nat(s(X)) :- nat(X).

sum(0, M, M) :- nat(M).
sum(s(M), N, s(L)) :- sum(M, N, L).

lt(0, s(M)) :- nat(M).
lt(s(N), s(M)) :- lt(N, M).

div(0, M, 0) :- lt(0, M).
div(N, M, 0) :- lt(N, M).
div(N, M, s(Q)) :- lt(0, M), sum(M, V, N), div(V, M, Q).
