/*

  Set covering in SICStus Prolog.

  Problem from 
  Katta G. Murty: "Optimization Models for Decision Making", page 302f
  http://ioe.engin.umich.edu/people/fac/books/murty/opti_model/junior-7.pdf
 
  10 senators making a committee, where there must at least be one 
  representative from each group:
  group:        senators:
  southern      1 2 3 4 5
  northern      6 7 8 9 10
  liberals      2 3 8 9 10
  conservative  1 5 6 7
  democrats     3 4 5 6 7 9
  republicans   1 2 8 10

  The objective is to minimize the number of senators.


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/set_covering3_model.mzn (model)
              http://www.hakank.org/minizinc/set_covering3.mzn (data)
  * Comet   : http://www.hakank.org/comet/set_covering3.co
  * ECLiPSe : http://www.hakank.org/eclipse/set_covering3.ecl


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
        belongs(Groups),
        set_covering3(Groups, MinVal,_),

        format('\nFinding all optimal solutions with MinVal ~d:\n', [MinVal]),
        findall(Xs, set_covering3(Groups,  MinVal,Xs), L),
        length(L, Len),
        write(L),nl,
        format('It was ~d solutions\n', [Len]).


set_covering3(Groups, MinVal, Xs) :-

        matrix(Groups,[_NumGroups,NumSenators]),

        % which senator to choose
        length(Xs,NumSenators),
        domain(Xs,0,1),

        % cover all groups with the senators
        ( foreach(Group,Groups),
          param(Xs)
        do
          scalar_product(Group,Xs,#>=,1,[])
        ),

        % objective: minimize the number of senators
        sum(Xs, #=, Z),
        Z #= MinVal,

        % either search for all solutions (for the minimum value) or
        % the optimal value
        (
            ground(MinVal) 
        -> 
            labeling([], Xs)
        ;
            labeling([minimize(Z)], Xs)
        ),

        write(z:Z),nl,
        write(x:Xs),nl,nl.



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
% 1 if a senator belongs to the group, 
% 0 if senator don't belong to the group
%
belongs([[1, 1, 1, 1, 1, 0, 0, 0, 0, 0],   % 1 southern
         [0, 0, 0, 0, 0, 1, 1, 1, 1, 1],   % 2 northern
         [0, 1, 1, 0, 0, 0, 0, 1, 1, 1],   % 3 liberals
         [1, 0, 0, 0, 1, 1, 1, 0, 0, 0],   % 4 conservative
         [0, 0, 1, 1, 1, 1, 1, 0, 1, 0],   % 5 democrats
         [1, 1, 0, 0, 0, 0, 0, 1, 0, 1]]). % 6 republicans
