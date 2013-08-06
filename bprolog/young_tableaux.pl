/*

  Young tableaux in B-Prolog.

  See 
  http://mathworld.wolfram.com/YoungTableau.html
  and
  http://en.wikipedia.org/wiki/Young_tableau
  """
  The partitions of 4 are
   {4}, {3,1}, {2,2}, {2,1,1}, {1,1,1,1}
 
  And the corresponding standard Young tableaux are:
 
  1.   1 2 3 4
 
  2.   1 2 3         1 2 4    1 3 4
       4             3        2
 
  3.   1 2           1 3
       3 4           2 4
 
  4    1 2           1 3      1 4 
       3             2        2 
       4             4        3
 
  5.   1
       2
       3
       4
  """  
  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


% 
% Show all solutions for N=6
%
go :- 
        N = 6,
        findall([], young_tableaux(N,_X,_P,1), L),
        format("It was ~d solutions.\n", [L^length]),
        nl.


%
% Get the number of solutions for N in 1..10.
%
go2 :-
        foreach(N in 1..10, [L,Len,_X,_P],ac(Lens,[]),
                (
                    findall([], young_tableaux(N,_X,_P,0), L),
                    length(L,Len),
                    format("It was ~d solutions.\n", [Len]),
                    Lens^1 = [Len|Lens^0],
                    nl
                )),
        reverse(Lens,LensR),
        writeln(lengths:LensR).


%
% Ensure that all values in Xs are different (if they are not N).
% 
alldifferent_except_N(Xs,N) :-
        Len @= Xs^length,
        foreach(I in 1..Len, J in 1..Len,
                        (I #\=J #/\ Xs[I] #\= N #/\ Xs[J] #\= N)  #=> 
                        (Xs[I] #\= Xs[J])
               ).



young_tableaux(N,X,P,Print) :-

        format('Young tableaux and partitions of order ~d\n', [N]),
        new_array(X, [N,N]),
        array_to_list(X, Vars),
        Vars :: 1..N+1,

        % the partition structure
        length(P,N),
        P :: 0..N+1,

        % 1..N is used exactly once (N+1 may be used many times)
        foreach(I in 1..N, count(I, Vars, #=, 1)),
        % alternative (but much slower for this purpose)
        % alldifferent_except_N(Vars,N+1),

        X[1,1] #= 1,
        % all rows and columns should be ordered
        foreach(I in 1..N, 
                [Rows, Columns],
                (
                    Rows    @= [X[I,J] : J in 1..N],
                    Columns @= [X[J,I] : J in 1..N],
                    increasing(Rows),
                    increasing(Columns)
                    % It would be nice if this work:
                    % increasing([X[I,J] : J in 1..N]), % rows
                    % increasing([X[J,I] : J in 1..N])  % columns
                )
        ),

        % calculate the structure (the partition)
        %   forall(i in 1..n) (
        %     p[i] = sum(j in 1..n) (bool2int(x[i,j] <= n))
        %   )
        foreach(I in 1..N, P[I] #= sum([ (X[I,J] #=< N)  : J in 1..N])),

        % P should be ordered
        decreasing(P),
        N #= sum(P),

        term_variables([Vars,P], Vars2),

        labeling([ff],Vars2),
        (Print =:= 1 ->
             writeln(p:P),
             pretty_print(X)
        ;
             true
        ).


increasing(List) :-
        foreach(I in 2..List^length, List[I-1] #=< List[I]).

decreasing(List) :-
        foreach(I in 2..List^length, List[I-1] #>= List[I]).


pretty_print(X) :-
        N @= X^length,
        foreach(I in 1..N,
                (foreach(J in 1..N, [XIJ],
                         (XIJ @= X[I,J],
                          (XIJ #=< N -> format('~2d', [XIJ]) ; true),
                          write(' ')
                         )),
                 % don't show "empty" lines
                 X[I,1] #=< N -> nl ; true
                )),
        nl.
