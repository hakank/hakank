/*

  Clock Triplet in SWI Prolog

  Problem formulation
  http://www.f1compiler.com/samples/Dean 20Clark 27s 20Problem.f1.html
  """
  Dean Clark's Problem (Clock Triplets Problem)
 
  The problem was originally posed by Dean Clark and then presented
  to a larger audience by Martin Gardner. 
 
  The problem was discussed in Dr. Dobbs's Journal, May 2004 in an article 
  by Timothy Rolfe. According to the article, in his August 1986 column for 
  Isaac Asimov's Science Fiction Magazine, Martin Gardner presented this problem:
  
    Now for a curious little combinatorial puzzle involving the twelve
    numbers on the face of a clock. Can you rearrange the numbers (keeping
    them in a circle) so no triplet of adjacent numbers has a sum higher 
    than 21? This is the smallest value that the highest sum of a triplet
    can have.
  ""


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        N = 12,
        Sum = 21,
        P = 3, %% number in each list to sum
        time(findall(Xs,clock_triplet(N, P, Sum, Xs),L)),
        maplist(writeln, L),
        length(L,Len),
        format("Sum: ~d Number of solutions: ~d\n",[Sum,Len]),
        nl.

%%
%% checks if 21 really is the smallest number (it is).
%%
go2 :-
   N = 12,
   Sum in 2..21,
   indomain(Sum),

   P = 3, % number in each list to sum
   findall(Xs,clock_triplet(N, P, Sum, Xs),L),
   length(L,Len),
   format("Sum: ~d Number of solutions: ~d\n",[Sum,Len]),
   (Len == 0
   ->
    fail
   ).
go2.


%%
%% This is slighly more general version where
%% P is the length of the tuples to sum
%% and N is the length of Xs (the numbers in the "clock")
%%
clock_triplet(N, P, Sum, Xs) :-

   length(Xs,N),
   Xs ins 1..N,

   all_distinct(Xs),

   numlist(0,N,Is),
   P1 #= P-1,
   numlist(0,P1,Ks),   
   maplist(check_sum(N,Sum,Xs,Ks),Is),

   
   %% symmetry breaking
   element(1,Xs,1),
   element(2,Xs,Xs2),
   element(N,Xs,XsN),   
   Xs2 #> XsN,

   labeling([min,bisect],Xs).

check_sum(N,Sum,Xs,Ks,I) :-
        check_sum_(Ks,Xs,I,N,0,KSum),
        KSum #=< Sum.

check_sum_([],_Xs,_I,_N,KSum,KSum).
check_sum_([K|Ks],Xs,I,N,KSum0,KSum) :-
        I1 #= 1+((I+K) mod N),
        element(I1,Xs,XsI1),
        KSum1 #= KSum0+XsI1,
        check_sum_(Ks,Xs,I,N,KSum1,KSum).
        
        