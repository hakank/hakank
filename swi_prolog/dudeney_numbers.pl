/*

  Dudeney numbers in SWI Prolog

  From Pierre Schaus blog post
  Dudeney number
   http://cp-is-fun.blogspot.com/2010/09/test-python.html
  """
  I discovered yesterday Dudeney Numbers
  A Dudeney Numbers is a positive integer that is a perfect cube such that the sum 
  of its decimal digits is equal to the cube root of the number. There are only six 
  Dudeney Numbers and those are very easy to find with CP.
  I made my first experience with google cp solver so find these numbers (model below) 
  and must say that I found it very convenient to build CP models in python!
  When you take a close look at the line: 
      solver.Add(sum([10**(n-i-1)*x[i] for i in range(n)]) == nb)
  It is difficult to argue that it is very far from dedicated 
  optimization languages!
  """
  
  Also see: http://en.wikipedia.org/wiki/Dudeney_number



  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        N = 6,
        findall([x=X,nb=NB,s=S], dudeney(N,X,NB,S),L),
        maplist(writeln,L),
        length(L,Len),
        writeln(len=Len),
        nl.


dudeney(N, X, NB, S) :-

   length(X,N),
   X ins 0..9,

   UB1 #= 10^N,
   NB in 1..UB1,
   UB2 #= 9*N+1,
   S in 1..UB2,

   NB #= S*S*S,
   %% NB #= sum([X[I]*10**(N-I) : I in 1..N]),
   numlist(1,N,Is),
   maplist(sum_d(N),X,Is,SS),
   sum(SS,#=,NB),
   sum(X,#=,S),

   labeling([],X).

sum_d(N,XI,I,S) :-
        S #= XI*10^(N-I).