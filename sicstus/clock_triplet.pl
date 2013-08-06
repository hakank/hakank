/*

  Clock Triplet Problem in SICStus Prolog.

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
  """

  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/clock_triplets.mzn


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        N = 12,
        Sum = 21,
        P = 3, % number in each list to sum
        findall(Xs,clock_triplet(N, P, Sum, Xs),L),
        write(L),nl,
        length(L,Len),
        format('It was ~d solutions for Sum = ~d.\n',[Len,Sum]),nl,
        fd_statistics.


%
% This is slighly more general version where
% P is the length of the tuples to sum
% and N is the length of Xs (the numbers in the "clock")
%
clock_triplet(N, P, Sum, Xs) :-

        length(Xs,N),
        domain(Xs,1,N),

        all_distinct(Xs),

        % symmetry breaking
        element(1,Xs,N),
        element(2,Xs,Xs2),
        element(N,Xs,XsN),
        Xs2 #> XsN,
        
        P1 is P-1,
        ( for(I,0,N),
          param(Xs,Sum,N,P1) do
              ( for(K,0,P1),
                fromto(List,Out,In,[]),
                param(Xs,N,I) do
                    I1 is 1+((I+K) mod N),
                    element(I1,Xs,XI1),
                    Out = [XI1|In]
                    ),
              sum(List,#=<,Sum)
        ),

        labeling([leftmost,bisect,up], Xs).


% inspired by the built in predicate nextto/2 
% nextto3(Xs,[A,B,C]) :-
%        append(_,[A,B,C|_], Xs).