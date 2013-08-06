/*

  Latin squares in SICStus Prolog.

  http://en.wikipedia.org/wiki/Latin_square:
  """
  A Latin square is an n X n table filled with n different symbols in
  such a way that each symbol occurs exactly once in each row and
  exactly once in each column. 
  """

  Compare with the following model:
  * http://www.hakank.org/eclipse/latin_squares.ecl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :-
        N = 33,
        latin_square(N, X),
        append(X,XList),
        labeling([ff,bisect,up],XList),
        pretty_print(X),
        fd_statistics.

matrix_element(X, I, J, Val) :-
        nth1(I, X, Row),
        element(J, Row, Val).


% From Mats Carlsson.
matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).


latin_square(N, X) :-
        matrix(X, [N,N]),
        append(X,XList),
        domain(XList,1,N),
        ( foreach(Row,X)
        do
          all_distinct(Row)
        ),
        transpose(X,XTransposed),
        ( foreach(Column,XTransposed)
        do
          all_distinct(Column)
        ),
        
        % symmetry breaking: value in row R must not be R
        ( for(R,1,N),
          param(X) do
              matrix_element(X,R,R,Val),
              Val #\= R
        ).

pretty_print(X) :-
        ( foreach(Row,X)
        do
          write(Row), nl
        ).

