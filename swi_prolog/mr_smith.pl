/*

  Mr Smith problem in SWI Prolog

  From an IF Prolog example (http://www.ifcomputer.de/)
  """
  The Smith family and their three children want to pay a visit but they
  do not all have the time to do so. Following are few hints who will go
  and who will not:
      o If Mr Smith comes, his wife will come too.
      o At least one of their two sons Matt and John will come.
      o Either Mrs Smith or Tim will come, but not both.
      o Either Tim and John will come, or neither will come.
      o If Matt comes, then John and his father will
        also come.
  """

  The answer should be:
    Mr_Smith_comes      =  0
    Mrs_Smith_comes     =  0
    Matt_comes          =  0
    John_comes          =  1
    Tim_comes           =  1

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        People = [mr_smith,mrs_smith,matt,john,tim],
        length(People,Len),
        findall(P,
                (mr_smith(L),
                 writeln(L),
                 between(1,Len,I),
                 element(I,L,1),
                 nth1(I,People,P)
                ),
                Sol),
        format("These will go to the party: ~w~n",[Sol]),
        nl.

mr_smith(L) :-

   L = [Mr_Smith,Mrs_Smith,Matt, John,Tim],
   L ins 0..1,

   % If Mr Smith comes, his wife will come too.
   Mr_Smith #==> Mrs_Smith,

   % At least one of their two sons Matt and John will come.
   Matt #\/ John,

   % Either Mrs Smith or Tim will come, but not both.
   Mrs_Smith + Tim #= 1,

   % Either Tim and John will come, or neither will come.
   Tim #= John,

   % If Matt comes, then John and his father will also come.
   Matt #==> (John #/\ Mr_Smith),

   label(L).
