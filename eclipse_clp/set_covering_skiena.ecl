/*

  Set covering in ECLiPSe.

 
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
        set_covering_skiena(Belongs, MinVal,_),

        printf("\nFinding all optimal solutions with MinVal %d:\n", [MinVal]),
        findall(X, set_covering_skiena(Belongs,  MinVal,X), L),
        length(L, Len),
        writeln(L),
        printf("It was %d solutions\n", [Len]).


set_covering_skiena(Belongs, MinVal, X) :-

        dim(Belongs,[NumSets,NumElements]),

        dim(X,[NumSets]),
        X :: 0..1,

        dim(TotElementsArray,[NumElements]),
        ( for(J,1,NumElements), param(Belongs,X,NumSets,TotElementsArray) do
             ( for(I,1,NumSets), 
               foreach(S,SSum),
               param(Belongs,X,J) do
                   S = X[I]*(Belongs[I,J] #>0)
             ),
             Sum #= sum(SSum),
             Sum #>= 1,
             TotElementsArray[J] #= Sum
        ),
        
        flatten_array(TotElementsArray,TotElementsArrayVar),
        TotElements #= sum(TotElementsArrayVar),


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
        writeln(tot_elements:TotElements),
        writeln(backtracks:Backtracks).


%
% The belong matrix:
%
belongs([]([](1,1,0,0,0,0,0,0,0,0,0,0),
           [](0,1,0,0,0,0,0,1,0,0,0,0),
           [](0,0,0,0,1,1,0,0,0,0,0,0),
           [](0,0,0,0,0,1,1,0,0,1,1,0),
           [](0,0,0,0,0,0,0,0,1,1,0,0),
           [](1,1,1,0,1,0,0,0,1,1,1,0),
           [](0,0,1,1,0,0,1,1,0,0,1,1))).
           