/*

  Set covering deployment in SICStus Prolog.

  From http://mathworld.wolfram.com/SetCoveringDeployment.html
  """
  Set covering deployment (sometimes written "set-covering deployment"
  and abbreviated SCDP for "set covering deployment problem") seeks 
  an optimal stationing of troops in a set of regions so that a 
  relatively small number of troop units can control a large 
  geographic region. ReVelle and Rosing (2000) first described 
  this in a study of Emperor Constantine the Great's mobile field 
  army placements to secure the Roman Empire.
  """


  Compare with the the following models:
  * MiniZinc: http://www.hakank.org/minizinc/set_covering_deployment.mzn
  * Comet   : http://www.hakank.org/comet/set_covering_deployment.co
  * Gecode  : http://www.hakank.org/gecode/set_covering_deployment.cpp
  * ECLiPSe : http://www.hakank.org/eclipse/set_covering_deployment.ecl

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
        problem(Matrix),
        armies(Armies),
        write(armies:Armies),nl,
        set_covering_deployment(Matrix, Armies, MinVal,_),

        format('\nFinding all optimal solutions with MinVal ~d:\n', [MinVal]),
        findall(Assignments, 
                set_covering_deployment(Matrix, Armies, MinVal, Assignments), 
                L),
        length(L, Len),
        nl,
        write(all_solutions:L),nl,
        format('\nIt was ~d solution(s)\n', [Len]).



set_covering_deployment(Matrix, Armies, MinVal, Assignments) :-

        %
        % adjacency matrix of the cities, order N
        %
        matrix(Matrix,[N,N]),

        % first army
        length(Xs,N),
        domain(Xs,0,1),

        % second army
        length(Ys,N),
        domain(Ys,0,1),


        %
        % Constraint 1: There is always an army in a city (+ maybe a backup)
        %               Or rather: Is there a backup, there must be an
        %               an army
        % 
        ( foreach(X,Xs),
          foreach(Y,Ys) do
              X #>= Y
        ),

        %
        % Constraint 2: There should always be an backup army near
        % every city
        %
        ( foreach(X,Xs),
          foreach(MatRow,Matrix),
          param(Ys) do
              ( foreach(Y,Ys),
                foreach(M,MatRow),
                fromto(0,In,Out,SS) do
                    Out #= In + Y*M
              ),
              X + SS #>= 1
        ),
        

        % objective: minimize the number of armies
        Z in 0..N,
        ( foreach(X,Xs),
          foreach(Y,Ys),
          fromto(0,In,Out,Z) do
              Out #= In + X + Y
        ),

        Z #= MinVal,

        % either search for all solutions (for the minimum value) or
        % the optimal value
        append(Xs,Ys,Vars),
        (
            ground(MinVal) 
        -> 
            labeling([],Vars)
        ;
            labeling([minimize(Z)],Vars)
        ),
        

        % convert X and Y to nicer representation
        assignments(Xs,Ys,Armies,Assignments),

        write(z:Z),nl,
        write(x:Xs),nl,
        write(y:Ys),nl,
        write(assigments:Assignments),nl,nl.


assignments(Xs,Ys,Armies,Assignments) :- 
        ( foreach(X,Xs),
          foreach(Y,Ys),
          count(N,1,_),
          fromto(Assignments,Out,In,[]),
          param(Armies) do
              Num #= X + Y,
              Num > 0 ->
              nth1(N,Armies,A),
              Out = [Num:A|In]
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



problem([[0, 1, 0, 1, 0, 0, 1, 1],
         [1, 0, 0, 1, 0, 0, 0, 0],
         [0, 0, 0, 0, 1, 1, 0, 0],
         [1, 1, 0, 0, 0, 0, 1, 0],
         [0, 0, 1, 0, 0, 1, 1, 0],
         [0, 0, 1, 0, 1, 0, 1, 1],
         [1, 0, 0, 1, 1, 1, 0, 1],
         [1, 0, 0, 0, 0, 1, 1, 0]]).

armies(['alexandria', 'asia_minor', 'britain', 'byzantium', 
        'gaul', 'iberia', 'rome', 'tunis']).
