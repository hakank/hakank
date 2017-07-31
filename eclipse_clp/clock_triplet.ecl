/*

  Clock Triplet Problem in ECLiPSe.

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

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/clock_triplets.mzn
  * SICStus Prolog: http://www.hakank.org/sicstus/clock_triplets.pl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(listut).

go :-
        N = 12,
        % Sum = 21,
        % checks if 21 really is the smallest number...
        Sum :: 2..21,
        indomain(Sum),

        P = 3, % number in each list to sum
        findall(Xs,clock_triplet(N, P, Sum, Xs),L),
        write(L),nl,
        % ( foreach(LL,L) do writeln(LL) ),
        length(L,Len),
        writeln(sum:Sum),
        writeln(solutions:Len),
        fail.

%
% This is slighly more general version where
% P is the length of the tuples to sum
% and N is the length of Xs (the numbers in the "clock")
%
clock_triplet(N, P, Sum, Xs) :-

        length(Xs,N),
        Xs :: 1..N,

        alldifferent(Xs),

        % symmetry breaking
        nth1(1,Xs,N),
        nth1(2,Xs,Xs2),
        nth1(N,Xs,XsN),
        Xs2 #> XsN,

        P1 is P-1,
        ( for(I,0,N),
          param(Xs,Sum,N,P1) do
              ( for(K,0,P1),
                fromto(List,Out,In,[]),
                param(Xs,N,I) do
                    I1 is 1+((I+K) mod N),
                    nth1(I1,Xs,XI1),
                    Out = [XI1|In]
                    ),
              writeln(list:List),
              sum(List) #=< Sum
        ),

        % search
        % labeling(Xs).
        search(Xs,0,first_fail,indomain_min, complete,[]).
