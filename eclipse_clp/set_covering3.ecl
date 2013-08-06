/*

  Set covering in ECLiPSe.

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


  Compare with the MiniZinc model 
  http://www.hakank.org/minizinc/set_covering3_model.mzn (model)
  http://www.hakank.org/minizinc/set_covering3.mzn (data)


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
%
go :-

        writeln("Find the optimal solution"),
        belongs(Belongs),
        set_covering3(Belongs, MinVal,_),

        printf("\nFinding all optimal solutions with MinVal %d:\n", [MinVal]),
        findall(X, set_covering3(Belongs,  MinVal,X), L),
        length(L, Len),
        writeln(L),
        printf("It was %d solutions\n", [Len]).


set_covering3(Belongs, MinVal, X) :-

        dim(Belongs,[NumGroups,NumSenators]),

        %
        % which senator to choose
        %
        dim(X,[NumSenators]),
        X :: 0..1,

        % 
        % cover all groups with the senators
        %
        ( for(I,1,NumGroups),
          param(NumSenators,X,Belongs) do
              ( for(J,1,NumSenators),
                fromto(0,In,Out,Sum),
                param(X,Belongs,I) do
                    Out #= In + X[J]*Belongs[I,J]
              ),
              Sum #>= 1
        ),


        %
        % objective: minimize the number of senators
        %
        flatten_array(X, Vars),
        Z #= sum(Vars),
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

        writeln(z:Z),
        writeln(x:X),
        writeln(backtracks:Backtracks).


%
% The belong matrix:
%
% 1 if a senator belongs to the group, 
% 0 if senator don't belong to the group
%
belongs([]([](1, 1, 1, 1, 1, 0, 0, 0, 0, 0),   % 1 southern
           [](0, 0, 0, 0, 0, 1, 1, 1, 1, 1),   % 2 northern
           [](0, 1, 1, 0, 0, 0, 0, 1, 1, 1),   % 3 liberals
           [](1, 0, 0, 0, 1, 1, 1, 0, 0, 0),   % 4 conservative
           [](0, 0, 1, 1, 1, 1, 1, 0, 1, 0),   % 5 democrats
           [](1, 1, 0, 0, 0, 0, 0, 1, 0, 1))). % 6 republicans
