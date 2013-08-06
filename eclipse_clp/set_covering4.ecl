/*

  Set covering, set partition in ECLiPSe.

  Example from Lundgren, Rönnqvist, Värbrand "Optimeringslära", page 408.
  
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
 


  Compare with the MiniZinc model 
  http://www.hakank.org/minizinc/set_covering4.mzn


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
:-lib(ic_search).
:-lib(branch_and_bound).
%:-lib(propia).


%
% First find the optimal value (MinVal), then find all the solutions with that value.
% Note: This handles both set partition and set covering.
%
go :-

        %
        % set partition
        %
        writeln("SET PARTITION"),
        writeln("Find the optimal solution"),
        problem(Costs,Alternatives),

        set_covering4(Costs, Alternatives, set_partition, MinVal,_),

        printf("\nFinding all optimal solutions with MinVal %d:\n", [MinVal]),
        findall(Assignments, set_covering4(Costs, Alternatives, set_partition, MinVal,Assignments), L),
        length(L, Len),
        writeln(all_solutions:L),
        printf("It was %d solution(s) (set partition)\n", [Len]),

        %
        % set covering
        %
        writeln("\nSET COVERING"),
        writeln("Find the optimal solution"),
        problem(Costs,Alternatives),

        set_covering4(Costs, Alternatives, set_covering, MinVal2,_),

        printf("\nFinding all optimal solutions with MinVal %d:\n", [MinVal2]),
        findall(Assignments2, set_covering4(Costs, Alternatives, set_covering, MinVal2,Assignments2), L2),
        length(L2, Len2),
        writeln(all_solutions:L2),
        printf("It was %d solution(s) (set covering)\n", [Len2]).


set_covering4(Costs, Alternatives, Type, MinVal, Assignments) :-

        % costs
        dim(Costs,[NumAlternatives]),

        % alternatives
        dim(Alternatives,[NumAlternatives,NumObjects]),

        % decision variable: which alternative to choose
        dim(X,[NumAlternatives]),


        %
        % which senator to choose
        %
        dim(X,[NumAlternatives]),
        X :: 0..1,

        % 
        % cover all groups with the senators
        %
        ( for(J,1,NumObjects),
          param(Alternatives,X,NumAlternatives,Type) do
              ( for(I,1,NumAlternatives),
                fromto(0,In,Out,Sum),
                param(X,J,Alternatives) do
                    Out #= In + X[I]*Alternatives[I,J]
              ),

              % which type? set partition or set covering?
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


        %
        % objective: minimize the number of senators
        %
        flatten_array(X, Vars),
        flatten_array(Costs,CostsList),
        Z #= Vars*CostsList,
        Z #= MinVal,

        %
        % either search for all solutions (for the minimum value) or
        % the optimal value
        %
        (
            ground(MinVal) 
        -> 
            search(Vars,0,first_fail,indomain,complete, [backtrack(Backtracks)])
        ;
            minimize(search(Vars,0,first_fail,indomain,complete,[backtrack(Backtracks)]),Z)
        ),

        (for(I,1,NumAlternatives), 
         fromto(Assignments, Out, In, []),
         param(X) do
             X[I] #= 1 -> 
             Out = [I|In]
        ;
             Out = In
        ),


        writeln(z:Z),
        writeln(x:X),
        writeln(assignements:Assignments),
        writeln(backtracks:Backtracks).


%
% cost and alternatives
%
problem([](19, 16, 18, 13, 15, 19, 15, 17, 16, 15), % costs
        []([](1,0,0,0,0,1,0,0),   % alternative 1    % alternatives 
           [](0,1,0,0,0,1,0,1),   % alternative 2
           [](1,0,0,1,0,0,1,0),   % alternative 3
           [](0,1,1,0,1,0,0,0),   % alternative 4
           [](0,1,0,0,1,0,0,0),   % alternative 5
           [](0,1,1,0,0,0,0,0),   % alternative 6
           [](0,1,1,1,0,0,0,0),   % alternative 7
           [](0,0,0,1,1,0,0,1),   % alternative 8
           [](0,0,1,0,0,1,0,1),   % alternative 9
           [](1,0,0,0,0,1,1,0))). % alternative 10
