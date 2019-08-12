/*

  Global constraint global contiguity in SWI Prolog

  From Global constraint catalog
  http://www.emn.fr/x-info/sdemasse/gccat/Cglobal_contiguity.html
  """
  Enforce all variables of the VARIABLES collection to be assigned to 
  0 or 1. In addition, all variables assigned to value 1 appear contiguously.
  """

  The implementation of global contiguity below was inspired by 
  Toby Walsh's presentation "Sliding Constraints"
     http://www.cse.unsw.edu.au/~tw/samos/slide.ppt
  where he defines it in terms of the global constraint slide.


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-

   N = 15,
   length(X,N),
   X ins 0..1,

   global_contiguity(X),
   
   labeling([],X),
   writeln(X),
   fail.

go.


%%
%% contiguity: all variables assigned to value 1 appear contiguously.
%%
global_contiguity(X) :-

   length(X,Len),
   length(Y,Len),
   Y ins 0..2,
    
   increasing(Y),
   maplist(global_contiguity_,X,Y).

global_contiguity_(X,Y) :-
        BX in 0..1,
        BY in 0..1,
        X #= 1 #<==> BX #= 1,
        Y #= 1 #<==> BY #= 1,
        BX #= BY.
