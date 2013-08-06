/*

  Set covering in ECLiPSe.

  Example 9.1-2, page 354ff, from Taha "Operations Research - An Introduction"
  Minimize the number of security telephones in street corners on a campus.

  AMPL model: http://taha.ineg.uark.edu/setcover.txt


  Compare with the MiniZinc model 
  http://www.hakank.org/minizinc/set_covering2.mzn


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
:-lib(ic_search).
:-lib(branch_and_bound).
%:-lib(propia).


%
% First find the optimal value (MinVal) i.e. the number of telephones
% placed. Then find all the solutions with that value.
%
go :-
        writeln("Find the optimal solution"),
        corners(N, Corners),
        set_covering2(N, Corners, MinVal,_),

        printf("\nFinding all optimal solutions with MinVal %d:\n", [MinVal]),
        findall(X, set_covering2(N, Corners,MinVal,X), L),
        length(L, Len),
        writeln(L),
        printf("It was %d solutions\n", [Len]).


set_covering2(N, Corners, MinVal, X) :-

        dim(Corners,[NumStreets,2]),
        
        % where to place the telephone
        dim(X,[N]),
        X :: 0..1,
        
        %
        % all streets must be covered
        %
        ( for(I,1,NumStreets),
          param(X,Corners) do
              X[Corners[I,1]] + X[Corners[I,2]] #>= 1
        ),

        %
        % objective: minimize the number of telephones
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


% corners of each street
%
% corners(NumberOfStreets, Corners)
%
corners(8, []([](1,2),
              [](2,3),
              [](4,5),
              [](7,8),
              [](6,7),
              [](2,6),
              [](1,6),
              [](4,7),
              [](2,4),
              [](5,8),
              [](3,5))).

