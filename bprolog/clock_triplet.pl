/*

  Clock Triplet Problem in B-Prolog.

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
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


go :-
        N = 12,
        Sum = 21,
        P = 3, % number in each list to sum
        findall(Xs,clock_triplet(N, P, Sum, Xs),L),
        foreach(LL in L, writeln(LL)),
        length(L,Len),
        format("Sum: ~d Number of solutions: ~d\n",[Sum,Len]).


go2 :-
        N = 12,
        % Sum = 21,
        % checks if 21 really is the smallest number...
        Sum :: 2..21,
        indomain(Sum),

        P = 3, % number in each list to sum
        findall(Xs,clock_triplet(N, P, Sum, Xs),L),
        % writeln(L),
        % foreach(LL in L, writeln(LL)),
        length(L,Len),
        format("Sum: ~d Number of solutions: ~d\n",[Sum,Len]),
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
        nth(1,Xs,N),
        nth(2,Xs,Xs2),
        nth(N,Xs,XsN),
        Xs2 #> XsN,

        foreach(I in 0..N,
                sum([XI1 : K in 0..P-1, [I1,XI1],
                     (I1 is 1+((I+K) mod N), 
                      nth(I1,Xs,XI1))]) #=< Sum
        ),

        labeling(Xs).
