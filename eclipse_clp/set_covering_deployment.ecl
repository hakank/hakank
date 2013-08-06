/*

  Set covering deployment in ECLiPSe.

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


  Compare with the MiniZinc model 
  http://www.hakank.org/minizinc/set_covering_deployment.mzn


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
        matrix(Matrix),
        armies(Armies),
        writeln(armies:Armies),
        set_covering_deployment(Matrix, Armies, MinVal,_),

        printf("\nFinding all optimal solutions with MinVal %d:\n", [MinVal]),
        findall(Assignments, set_covering_deployment(Matrix, Armies, MinVal,Assignments), L),
        length(L, Len),
        nl,
        writeln(all_solutions:L),
        printf("\nIt was %d solution(s)\n", [Len]).



set_covering_deployment(Matrix, Armies, MinVal, Assignments) :-

        %
        % adjacency matrix of the cities, order N
        %
        dim(Matrix,[N,N]),

        % first army
        dim(X,[N]),
        X:: 0..1,

        % second army
        dim(Y,[N]),
        Y:: 0..1,


        %
        % Constraint 1: There is always an army in a city (+ maybe a backup)
        %               Or rather: Is there a backup, there must be an
        %               an army
        % 
        ( for(I,1,N), param(X,Y) do 
              X[I] #>= Y[I]
        ),

        %
        % Constraint 2: There should always be an backup army near
        % every city
        %
        ( for(I,1,N),
          param(X,Y,Matrix,N) do
              ( for(J,1,N), 
                fromto(0,In,Out,Sum),
                param(I,Matrix,Y) do
                    Out #= In + Y[J]*(Matrix[I,J] #= 1)
              ),
              X[I] + Sum #>= 1
        ),

        %
        % objective: minimize the number of armies
        %
        Z :: 0..N,
        (for(I,1,N),
         fromto(0,In,Out,Z),
         param(X,Y) do
             Out #= In + X[I]+Y[I]
        ),

        Z #= MinVal,


        term_variables([X,Y], Vars),

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


        % convert X and Y to nicer representation
        (for(I,1,N), 
         fromto(Assignments, Out, In, []),
         param(X,Y,Armies) do
             (X[I] or Y[I]) #= 1,
             Num #= X[I] + Y[I] % number of armies in this city
        -> 
             A is Armies[I],
             Out = [Num:A|In]
        ; 
             Out = In

        ),


        writeln(z:Z),
        writeln(x:X),
        writeln(y:Y),
        writeln(assigments:Assignments),
        writeln(backtracks:Backtracks).


matrix([]([](0, 1, 0, 1, 0, 0, 1, 1),
          [](1, 0, 0, 1, 0, 0, 0, 0),
          [](0, 0, 0, 0, 1, 1, 0, 0),
          [](1, 1, 0, 0, 0, 0, 1, 0),
          [](0, 0, 1, 0, 0, 1, 1, 0),
          [](0, 0, 1, 0, 1, 0, 1, 1),
          [](1, 0, 0, 1, 1, 1, 0, 1),
          [](1, 0, 0, 0, 0, 1, 1, 0))).

armies([]("alexandria", "asia_minor", "britain", "byzantium", "gaul", "iberia", "rome", "tunis")).
