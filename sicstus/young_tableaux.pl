/*

  Young tableaux  in SICStus Prolog.

  
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
  

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/young_tableaux.mzn
  * Choco   : http://www.hakank.org/choco/YoungTableuax.java
  * JaCoP   : http://www.hakank.org/JaCoP/YoungTableuax.java
  * Comet   : http://www.hakank.org/comet/young_tableaux.co
  * Gecode  : http://www.hakank.org/gecode/young_tableaux.cpp
  * ECLiPSE : http://www.hakank.org/eclipse/young_tableaux.ecl
  * Tailor/Essence': http://www.hakank.org/tailor/young_tableaux.eprime


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :-
        N = 4,
        ( N > 5 -> 
              Write = false
        ;
              Write = true
        ),
        findall(1,young_tableaux(N,Write),L),
        length(L,Len),
        nl,
        write('Number of partitions':Len),nl,nl,
        fd_statistics.


young_tableaux(N,Write) :-
        format("Young tableaux and partitions of order ~d\n", [N]),

        matrix(X,[N,N]),
        transpose(X, XTransposed),
        append(X,XList),
        N1 is N+1,
        domain(XList, 1, N1),

        % the partition structure
        length(P,N),
        domain(P,0,N1),

        % 1..N is used exactly once 
        % (N+1 may, on the other hand, be used many times)
        ( for(I,1,N), 
          param(XList) 
        do
          count(I,XList,#=,1)
        ),

        % X[1,1] #= 1,
        matrix_element(X,1,1,1),

        % all rows and columns should be ordered
        ( foreach(Row, X) do
              % my_ordered_lt(Row)
              my_ordered(#=<, Row)
        ),
        ( foreach(Column, XTransposed) do
              % my_ordered_lt(Column)
              my_ordered(#=<, Column)

        ),

        % calculate the partition structure
        ( for(I,1,N), 
          param(X,N,P) do
              ( for(J,1,N), 
                param(X,N,I),
                fromto(0,In,Out,Sum) do
                    matrix_element(X,I,J,XIJ),
                    Z in 0..1,
                    XIJ #=< N #<=> Z #= 1,
                    Out #= In + Z
              ),
              element(I,P,Sum)
        ),

        sum(P,#=, N),
        % P should be ordered
        % my_ordered_gt(P),
        my_ordered(#>=,P),

        % search
        append(XList,P, Vars2),
        labeling([max,enum,down],Vars2),

        % output
        ( 
            Write == true -> 
                write('Partition: '),
                ( foreach(PP,P) do
                      format('~d ',[PP])
                ),
                nl,
                pretty_print(X)
        ;
                true
        ).


% we use this instead
my_ordered(P,List) :-
        ( fromto(List, [This,Next | Rest], [Next|Rest],[_]),
          param(P)
        do
          call(P,This,Next)
        ).

% my_ordered_lt(List) :-
%         ( List = [] -> 
%               true
%         ;
%               ( fromto(List, [This,Next | Rest], [Next|Rest],[_])
%               do
%                 This #=< Next
%               )
%         ).

% my_ordered_gt(List) :-
%         ( List = [] -> 
%               true
%         ;
%               ( fromto(List, [This,Next | Rest], [Next|Rest],[_])
%               do
%                 This #>= Next
%               )
%         ).

pretty_print(X) :-
        length(X, N),
        ( for(I, 1, N), 
          param(X, N) do
              ( for(J, 1, N), param(X,I,N) do
                    matrix_element(X,I,J,XIJ),
                    (XIJ #=< N -> format(' ~d', [XIJ]) ; true),
                    write(' ')
              ),
              % don't show "empty" lines
              matrix_element(X,I,1,XI1),
              XI1 #=< N -> nl ; true
        ),nl.


matrix_element(Matrix, I, J, Val) :-
        nth1(I, Matrix, Row),
        element(J, Row, Val).


% From Mats Carlsson.
matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).

