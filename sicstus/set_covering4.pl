/*

  Set covering, set partition in SICStus Prolog.

  Example from Lundgren, Rönnqvist, Värbrand 
  "Optimeringslära" ["Optimization"], page 408.


  We want to minimize the cost of the alternatives which covers all the 
  objects, i.e. all objects must be choosen. The requirement is than an object 
  may be selected _exactly_ once.
 
  Alternative        Cost        Object
  1                  19           1,6
  2                  16           2,6,8
  3                  18           1,4,7
  4                  13           2,3,5
  5                  15           2,5
  6                  19           2,3
  7                  15           2,3,4
  8                  17           4,5,8
  9                  16           3,6,8
  10                 15           1,6,7
 
  The problem has a unique solution of z = 49 where alternatives 
  3, 5, and 9 is selected. 
 
  If we, however, allow that an object is selected more than one time, 
  then the solution is z = 45 (i.e. less cost than the first problem),
  and the alternatives 4, 8, and 10 is selected, where object 5 is 
  selected twice (alt. 4 and 8). It's an unique solution as well.
 

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/set_covering4.mzn
  * Comet   : http://www.hakank.org/comet/set_covering4.co
  * ECLiPSe : http://www.hakank.org/eclipse/set_covering4.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

%
% First find the optimal value (MinVal), then find all the solutions 
% with that value.
%
% Note: This model handles both the set partition and 
%       the set covering problem.
%
go :-

        %
        % set partition
        %
        write('SET PARTITION'),nl,
        write('Find the optimal solution'),nl,
        problem(Costs,Alternatives),

        set_covering4(Costs, Alternatives, set_partition, MinVal,_),

        format('\nFinding all optimal solutions with MinVal ~d:\n', [MinVal]),
        findall(Assignments, 
                set_covering4(Costs, Alternatives, set_partition,
                              MinVal,Assignments), 
                L),
        length(L, Len),
        write(all_solutions:L),nl,
        format('It was ~d solution(s) (set partition)\n', [Len]),


        %
        % set covering
        %
        write('\nSET COVERING\n'),nl,
        write('Find the optimal solution'),nl,
        problem(Costs,Alternatives),

        set_covering4(Costs, Alternatives, set_covering, MinVal2,_),

        format('\nFinding all optimal solutions with MinVal ~d:\n', [MinVal2]),
        findall(Assignments2, 
                set_covering4(Costs, Alternatives, set_covering,
                              MinVal2,Assignments2), 
                L2),
        length(L2, Len2),
        write(all_solutions:L2),nl,
        format('It was ~d solution(s) (set covering)\n', [Len2]),
        fd_statistics.


set_covering4(Costs, Alternatives, Type, MinVal, Assignments) :-

        % costs
        length(Costs,NumAlternatives),

        % alternatives
        matrix(Alternatives,[NumAlternatives,_NumObjects]),

        % decision variable: which alternative to choose
        length(Xs,NumAlternatives),


        % which alternative to choose
        length(Xs,NumAlternatives),
        domain(Xs,0,1),

        % cover all objects with the alternatices
        % (note: uses the transpose)
        transpose(Alternatives,AlternativesT), 
        ( foreach(Alternative,AlternativesT),
          param(Xs,Type) do
              scalar_product(Alternative,Xs,#=,Sum),

              % set partition or set covering?
              (
                  Type = set_partition
              -> 
                  % all objects must be covered _exactly_ once
                  % (set partition)
                  Sum #= 1
              ;
                  % variant: all objects must be covered _at least_ once
                  % (set covering)
                  Sum #>= 1
              )
        ),

        % objective: minimize the number of alternatives
        scalar_product(Costs,Xs,#=,Z),
        Z #= MinVal,

        %
        % either search for all solutions (for the minimum value) or
        % the optimal value
        %
        (
            ground(MinVal) 
        -> 
            labeling([], Xs)
        ;
            labeling([minimize(Z)], Xs)
        ),

        % which assignments was made?
        assignments(Xs, Assignments),

        write(z:Z),nl,
        write(x:Xs),nl,
        write(assignements:Assignments),nl,nl.


assignments(Xs,Assignments) :-
        ( foreach(X, Xs),
          count(N,1,_),
          fromto(Assignments, Out, In, []) 
        do
          X #= 1 -> 
              Out = [N|In]
        ;
              Out = In
        ).


% From Mats Carlsson
matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).



%
% Costs and the alternatives
%
problem([19, 16, 18, 13, 15, 19, 15, 17, 16, 15], % costs
        [[1,0,0,0,0,1,0,0],   % alternative 1    % alternatives 
         [0,1,0,0,0,1,0,1],   % alternative 2
         [1,0,0,1,0,0,1,0],   % alternative 3
         [0,1,1,0,1,0,0,0],   % alternative 4
         [0,1,0,0,1,0,0,0],   % alternative 5
         [0,1,1,0,0,0,0,0],   % alternative 6
         [0,1,1,1,0,0,0,0],   % alternative 7
         [0,0,0,1,1,0,0,1],   % alternative 8
         [0,0,1,0,0,1,0,1],   % alternative 9
         [1,0,0,0,0,1,1,0]]). % alternative 10
