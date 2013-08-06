/*

  Global constraint global cardinaliry in ECLiPSe.

  See Global Constraint Catalog
  http://www.emn.fr/x-info/sdemasse/gccat/Cglobal_cardinality.html

  Note: The version here is limited. See below for details.

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
:-lib(listut).
%:-lib(ic_search).
%:-lib(branch_and_bound).
%:-lib(propia).



%
% Both A and Gcc are arrays.
% 
% This version is bidirectional but limited:
% 
% The array A can contain only values 1..Max (i.e. the length of Gcc).
% This means that the caller must know the max values of A.
% Or rather: if A contains another values they will not be counted.
% 
global_cardinality(A, Gcc) :-
        dim(A,[Len]),
        dim(Gcc,[Max]),
        Gcc :: 0..Len,
        (for(I,1,Max), param(Gcc,A) do
             ic_global:occurrences(I,A,C),
             Gcc[I] #= C
        ).


%
% just show all variants of size 10.
%
go :-
        N = 5,
        dim(A,[N]),
        A :: 1..N,
        % A = [](1,2,1,1,1,1,1,2,1,1),
        dim(GCC,[N]),
        GCC :: 0..N,
        global_cardinality(A, GCC),
        labeling(A),
        labeling(GCC),
        writeln(a:A),
        writeln(gcc:GCC), nl,fail.
