/*

  Set covering in SICStus Prolog.

  Example from Steven Skiena, The Stony Brook Algorithm Repository
  http://www.cs.sunysb.edu/~algorith/files/set-cover.shtml
  """
  Input Description: A set of subsets S_1, ..., S_m of the 
  universal set U = {1,...,n}.
  
  Problem: What is the smallest subset of subsets T subset S such 
  that \cup_{t_i in T} t_i = U?
  """
  Data is from the pictures INPUT/OUTPUT.


  Compare with this models:
  * MiniZinc: http://www.hakank.org/minizinc/set_covering_skiena.mzn
  * Comet   : http://www.hakank.org/comet/set_covering_skiena.co
  * ECLiPSe : http://www.hakank.org/eclipse/set_covering_skiena.ecl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

%
% First find the optimal value (MinVal), then find all the solutions with that value.
%
go :-

        write('Find the optimal solution'),nl,
        belongs(Belongs),
        set_covering_skiena(Belongs, MinVal,_),

        format('\nFinding all optimal solutions with MinVal ~d:\n', [MinVal]),
        findall(X, set_covering_skiena(Belongs,  MinVal,X), L),
        length(L, Len),
        write(L),nl,
        format('It was ~d solutions\n', [Len]),nl,
        fd_statistics.


set_covering_skiena(Belongs, MinVal, X) :-

        matrix(Belongs,[NumSets,NumElements]),

        length(X,NumSets),
        domain(X,0,1),

        transpose(Belongs,BelongsTransposed),
        ( foreach(Belong,BelongsTransposed),
          param(X) do 
              ( foreach(B,Belong),
                fromto(0,In,Out,Sum),
                foreach(XX,X) do
                    Reif in 0..1,
                    B #>0 #<=> Reif#=1, 
                    Out #= In + XX*Reif
              ),
              Sum #>= 1
        ),

        sum(X,#=,Z),
        Z #= MinVal,

        % either search for all solutions (for the minimum value) or
        % the optimal value
        (
            ground(MinVal) 
        -> 
            labeling([],X)
        ;
            labeling([minimize(Z)], X)
        ),

        write(z:Z),nl,
        write(x:X),nl.



matrix_element(X, I, J, Val) :-
        nth1(I, X, Row),
        element(J, Row, Val).


% From Mats Carlsson
matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).



%
% The belong matrix:
%
belongs([[1,1,0,0,0,0,0,0,0,0,0,0],
         [0,1,0,0,0,0,0,1,0,0,0,0],
         [0,0,0,0,1,1,0,0,0,0,0,0],
         [0,0,0,0,0,1,1,0,0,1,1,0],
         [0,0,0,0,0,0,0,0,1,1,0,0],
         [1,1,1,0,1,0,0,0,1,1,1,0],
         [0,0,1,1,0,0,1,1,0,0,1,1]]).
           