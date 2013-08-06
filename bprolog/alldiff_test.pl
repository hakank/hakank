/*

  in B-Prolog.


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


go :-
        N = 5,
        length(X, N),
        X :: 1..N,
        
        % This works of course:
        % alldifferent(X),

        % I would expect this to work, but it don't.
        % alldifferent([X[I] : I in 1..N]),
        % Neither does this...
        % alldifferent_me([X[I] : I in 1..N]),

        % These two don't work at all: invalid_boolean_constraint_expression
        % B #= 1,
        % (B #<=> alldifferent([X[I] : I in 1..N])),
        % 
        % (B #<=> alldifferent_me([X[I] : I in 1..N])),

        Tmp @= [X[I] : I in 1..N],
        alldifferent(Tmp),

        labeling(X),
        writeln(X).


alldifferent_me(L) :-
        length(L, Len),
        foreach(I in 1..Len, J in I+1..Len, L[I] #\= L[J]).


alldifferent_me2(L) :-
        length(L, Len),
        1 #<=> foreach(I in 1..Len, J in I+1..Len, L[I] #\= L[J]).
