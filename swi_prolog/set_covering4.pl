/*

  Set covering and set partition in SWI Prolog

  Example from Lundgren, Ronnqvist, Varbrand "Optimeringslora", page 408.
  [This is a Swedish book about Operational Research.]
  
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


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).


%
% First find the optimal value (MinVal), then find all the solutions with that value.
% Note: This handles both set partition and set covering.
%
go :-

        %
        % set partition
        %
        writeln("\nSET PARTITION"),
        writeln("Find the optimal solution"),
        problem(Costs,Alternatives),

        set_covering4(Costs, Alternatives, set_partition, MinVal, _),

        format("\nFinding all optimal solutions with MinVal ~d:\n", [MinVal]),
        findall(Assignments, 
                set_covering4(Costs, Alternatives, set_partition,MinVal,Assignments),L),
        length(L,Len),
        writeln(all_solutions=L),
        format("It was ~d solution(s) (Set partition)\n\n", [Len]),


        %
        % Set covering
        %
        writeln("\nSET COVERING"),
        writeln("Find the optimal solution\n"),

        set_covering4(Costs, Alternatives, set_covering, MinVal2, _),

        format("\nFinding all optimal solutions with MinVal ~d:\n", [MinVal2]),
        findall(Assignments2, 
                set_covering4(Costs, Alternatives, set_covering,MinVal2,Assignments2),
                L2),
        length(L2, Len2),
        writeln(all_solutions=L2),
        format("It was ~d solution(s) (Set covering)\n\n", [Len2]).



set_covering4(Costs, Alternatives, Type, MinVal, Assignments) :-

        % get the dimensions
        transpose(Alternatives,AlternativesT),
        length(Alternatives,NumAlternatives),

        % which alternatives to choose
        length(X,NumAlternatives),
        X ins 0..1,

        % set partition or set covering?
        (
         Type == set_partition
        ->
         % set_partition: all objects covered exactly once
         Rel = #=
        ;
         % set_covering: all objects covered exactly once
         Rel = #>=

        ),
        maplist(cover_groups(X,Rel,1),AlternativesT),
        
        %
        % objective: minimize the number of senators
        %
        scalar_product(Costs,X,#=,MinVal),

        %
        % either search for all solutions (for the minimum value) or
        % the optimal value
        %
        (
         ground(MinVal)
        ->
          label(X)
        ;
         labeling([min(MinVal)], X)
        ),
        findall(I,
                (between(1,NumAlternatives,I),element(I,X,1)),
                Assignments).


cover_groups(X,Rel,Num,Group) :-
        scalar_product(Group,X,Rel,Num).


%
% cost and alternatives
%
problem(Costs, Alternatives) :- 
        Costs = [19, 16, 18, 13, 15, 19, 15, 17, 16, 15], % costs
        Alternatives = 
        [[1,0,0,0,0,1,0,0],     % alternative 1    % alternatives 
         [0,1,0,0,0,1,0,1],     % alternative 2
         [1,0,0,1,0,0,1,0],     % alternative 3
         [0,1,1,0,1,0,0,0],     % alternative 4
         [0,1,0,0,1,0,0,0],     % alternative 5
         [0,1,1,0,0,0,0,0],     % alternative 6
         [0,1,1,1,0,0,0,0],     % alternative 7
         [0,0,0,1,1,0,0,1],     % alternative 8
         [0,0,1,0,0,1,0,1],     % alternative 9
         [1,0,0,0,0,1,1,0]].    % alternative 10
