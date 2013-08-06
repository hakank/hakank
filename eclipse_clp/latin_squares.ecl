/*

  Latin squares in ECLiPSe.

  http://en.wikipedia.org/wiki/Latin_square:
  """
  A Latin square is an n × n table filled with n different symbols in
  such a way that each symbol occurs exactly once in each row and
  exactly once in each column. 
 """


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).


latin_square(N, X) :-
        dim(X, [N,N]),
        X[1..N,1..N] :: 1..N,
        (
            for(I, 1, N),
            param(X, N)
        do
            ic_global:alldifferent(X[1..N, I]),
            ic_global:alldifferent(X[I, 1..N])
        ).

pretty_print(X) :-
        dim(X, [N,N]),
        ( for(I, 1, N), param(X, N) do
            ( for(J, 1, N), param(X, I) do
                XX is X[I,J],
                printf("%2d", XX),
                write(" ")
            ),
            nl
        ),nl.        

go :-
        N = 13,
        latin_square(N, X),
        search(X, 0, smallest, indomain_middle, complete, []),
        pretty_print(X).