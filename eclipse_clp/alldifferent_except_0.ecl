/*

  Global constraint (decomposition) all different except 0 in ECLiPSe.

  Decomposition of the global constraint alldifferent except 0.
  
  From Global constraint catalogue:
  http://www.emn.fr/x-info/sdemasse/gccat/sec4.6.html
  """ 
  Enforce all variables of the collection VARIABLES to take distinct values, except those 
  variables that are assigned to 0.
 
  Example
    (<5, 0, 1, 9, 0, 3>)
 
  The alldifferent_except_0 constraint holds since all the values (that are different from 0) 
  5, 1, 9 and 3 are distinct.
  """

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:- lib(propia).

% copy the content of array A to array B
copy_array(A,B, Len) :-
        ( for(I, 1, Len), param(A, B) do B[I] = A[I] ).

writeall(Xs) :-
        ( foreach(El, Xs) do writeln(El)).

%
% just a silly decomposition of alldifferent
%
alldifferent_decomp(Xs) :-
        dim(Xs, [Len]),
        ( for(I, 1, Len) * for(J, 1, Len), param(Xs) do 
              I \= J
        ->
          Xs[I] #\= Xs[J]
        ; 
          true
        ),
        labeling(Xs)
        .


%
% alldifferent_except_0(Xs):
%
% all elements in Xs != 0 must be pairwise different.
%
alldifferent_except_0(Xs) :-
        dim(Xs, [Len]),
        labeling(Xs),
        ( for(I, 1, Len) * for(J, 1, Len), param(Xs) do 
            ( I \= J, Xs[I] #\= 0, Xs[J] #\= 0 )
        ->
          Xs[I] #\= Xs[J]
        ; 
          true
        ).

go :-
        Len = 3,
        dim(Ys, [Len]),
        Ys::[0..4],
        findall(Ys,  alldifferent_except_0(Ys), L),
        length(L, LLen),
        writeln(len:LLen),
        % and show all as proper lists
        (foreach(X, L) do
             collection_to_list(X, XList),
             writeln(XList)
        ).


go2 :-
        Len = 3,
        dim(Xs, [Len]),
        Xs::[0..3],
        alldifferent_except_0(Xs), 
        labeling(Xs),
        writeln(Xs).

go3 :-
        Xs = [](5, 0, 1, 9, 0, 3),
        alldifferent_except_0(Xs),
        writeln(Xs).
        

labelx([X|Xs]) :-
        indomain(X),
        labelx(Xs).

