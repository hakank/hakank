/*

  Global constraint global cardinality in B-Prolog.

  See Global Constraint Catalog
  http://www.emn.fr/x-info/sdemasse/gccat/Cglobal_cardinality.html
  """
  Example:
  <3,3,8,6>,
   (val-3 noccurrence-2,
    val−5 noccurrence-0,
    val-6 noccurrence-1)

  The global_cardinality constraint holds since values 3, 5 and 6 
  respectively occur 2, 0 and 1 times within the collection 
  <3,3,8,6> and since no constraint was specified for value 8.
  """

  Notes: 
     - The version here is limited. See below for details.
     - B-Prolog has already a global_cardinality/2, but the details
       of the parameters are different.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

%
% just show all variants of size 5.
%
go :-
        N = 5,
        length(A,N),
        A :: 1..N,
        % A = [1,2,1,1,1],

        length(GCC,N),
        GCC :: 0..N,
        global_cardinality(A, GCC),

        term_variables([A,GCC], Vars),
        labeling(Vars),

        writeln(a:A),
        writeln(gcc:GCC), nl,fail.

%
% Both A and Gcc are arrays.
% 
% This version is bidirectional but limited:
% 
% The list A can contain only values 1..Max (i.e. the length of Gcc).
% This means that the caller must know the max values of A.
% Or rather: if A contains another values they will not be counted.
% 
global_cardinality(A, Gcc) :-
        length(A,Len),
        length(Gcc,Max),
        Gcc :: 0..Len,
        foreach(I in 1..Max, count(I,A,#=,Gcc[I])).
