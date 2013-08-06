/*

  Global constraint global cardinaliry in ECLiPSe.

  Implementing global_cardinality(var_array, values, occurrences)
     

  See Global Constraint Catalog
  http://www.emn.fr/x-info/sdemasse/gccat/Cglobal_cardinality.html

  Note: The version here is limited. See below for details.

  Compare with the predicate global_cardinality/2
  http://www.hakank.org/eclipse/global_cardinality.ecl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
:-lib(listut).
:-lib(ic_search).


%
% global_cardinality(?X, +Values,?Gcc)
%
% where Gcc is an array of the occurrences of 
% the Values in Array.
% 
% Note that Values must be instantiated
% 
global_cardinality(X, Values, Gcc) :-
        ic_global:alldifferent(Values), % sanity check
        (foreacharg(I,Values), param(Gcc,X) do
             Gcc[I] #= ic_global:occurrences(I,X)
        ).

go :-
        %
        % here we have an array of length N which can
        % take the values from 1..ValuesLen
        %
        N = 6,
        ValuesLen = 4,
        dim(A,[N]),
        A :: 1..ValuesLen,
        % A = [](1,2,3,4,1,2),

        % this in no fun :-)
        % ic_global:ordered(=<, A),

        % The values of interest
        dim(Values , [ValuesLen]),
        ( foreacharg(V, Values), count(I,1,_) do
              V = I
        ),
        dim(GCC,[ValuesLen]),
        GCC :: 0..N,

        % and we hardcode the occurrences
        GCC = [](2,1,0,3),
        % ic_global:ordered(=<, GCC),

        global_cardinality(A, Values, GCC),

        term_variables([A,GCC,Values], Vars),
        search(Vars,0,first_fail, indomain_min, complete, []),

        writeln(a:A),
        writeln(values:Values),
        writeln(gcc:GCC), 
        nl,
        fail.
