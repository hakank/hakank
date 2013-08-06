/*

  Global constraint (decomposition) all different except 0 in B-Prolog.

  Decomposition of the global constraint alldifferent except 0.
  
  From Global constraint catalogue:
  http://www.emn.fr/x-info/sdemasse/gccat/sec4.6.html
  """ 
  Enforce all variables of the collection VARIABLES to take distinct values, except those 
  variables that are assigned to 0.
 
  Example
    (<5, 0, 1, 9, 0, 3>)
 
  The alldifferent_except_0 constraint holds since all the values 
  (that are different from 0) 5, 1, 9 and 3 are distinct.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


%
% alldifferent_except_0(Xs):
%
% requires that all elements in Xs != 0 must be pairwise 
% different.
%
alldifferent_except_0(Xs) :-
        Len @= Xs^length,
        foreach(I in 1..Len, J in 1..Len,
                        (I #\=J #/\ Xs[I] #\= 0 #/\ Xs[J] #\= 0)  #=> 
                        (Xs[I] #\= Xs[J])
               ).

go :-
        Len = 3,
        length(Ys, Len),
        Ys :: 0..4,
        findall(Ys,  (alldifferent_except_0(Ys), labeling(Ys)), L),
        length(L, LLen),
        writeln(len:LLen),
        % and show all as proper lists
        foreach(X in L, writeln(X)).


go2 :-
        Len = 3,
        length(Xs, Len),
        Xs :: 0..2,
        findall(Xs,  (alldifferent_except_0(Xs), labeling(Xs)), L),
        foreach(LL in L, writeln(LL)).

%
% A list with one blank, for alldifferent_except_0 to fill out.
%
go3 :-
        Xs = [5, 0, 1, _, 0, 3],
        Xs :: 0..9,
        findall(Xs, (alldifferent_except_0(Xs), labeling(Xs)),L),
        foreach(LL in L, writeln(LL)).

