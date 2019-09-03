/*

  Pair divides the sum puzzle in SWI Prolog

  From comp.lang.prolog
  """
  Date: Sat, Feb 28 2009 3:55 am
  From: Nick Wedd

  Here is a puzzle which I found surprisingly easy to program Prolog to
  generate solutions to.  If any of you teach Prolog to students, you
  might use it as an example (like the goat-wolf-cabbage thing).

  Find a set of four distinct positive integers such that, for every pair
  of them, their difference divides their sum.

  Find lots of such sets.

  As above, but sets of five distinct positive integers.
  
  As above, but sets of six ...
  """

  (This is a port of my B-Prolog model
   http://www.hakank.org/bprolog/pair_divides_the_sum.pl  
   which was a port of my MiniZinc model:
   http://www.hakank.org/minizinc/pair_divides_the_sum.mzn
  )

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        MaxVal = 100,
        findall([X,Z], (problem(4,MaxVal,X,Z),writeln(X=Z)), L),
        length(L,Len),
        writeln(len=Len),
                
        nl.

go2 :-
        MaxVal = 100,
        between(1,7,N),
        time(findall(_,(problem(N,MaxVal,_X,_Z)),L)),
        length(L,Len),
        writeln(N=Len),
        fail,
        nl.

go2.

%
% There are no solutions for 8 or 9 (for MaxVal=100)
%
problem(N, MaxVal, X, Z) :-
       
        length(X, N),
        X ins 1..MaxVal,

        % MaxValN #= MaxVal*N,
        % Z in N..MaxValN,

        all_different(X),
        increasing(X),

        sum(X,#=,Z),
        Z mod N #= 0,
        %%  foreach(I in 1..N, J in I+1..N, Z mod abs(X[I]-X[J]) #= 0),
        findall(I,
                between(1,N,I),
                Is),
        maplist(check(N,X,Z),Is),
        
        flatten([X,Z], Vars),
        labeling([ffc,enum], Vars).
                

%%  foreach(I in 1..N, J in I+1..N, Z mod abs(X[I]-X[J]) #= 0),
check(N,X,Z,I) :-
        element(I,X,XI),
        I1 #= I+1,
        (
         I1 #< N
        -> 
         numlist(I1,N,Js),
         maplist(check_mod(Z,X,XI),Js)
        ;
         true
        ).

check_mod(Z,X,XI,J) :-
        element(J,X,XJ),
        Z mod abs(XI-XJ) #= 0.