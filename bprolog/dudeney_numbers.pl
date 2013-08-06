/*

  Dudeney numbers in B-Prolog.

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
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


go :-
        N = 6,

        findall([X,NB,S], dudeney(N,X,NB,S), L),
        foreach([X,NB,S] in L,
                (writeln(x:X),
                 writeln(nb:NB),
                 writeln(s:S),
                 nl
                )),
        length(L, Len),
        writeln(len:Len),
        nl.


dudeney(N, X, NB, S) :-
        length(X,N),
        X :: 0..9,

        NB :: 1..10**N,
        S :: 1..9*N+1,

        NB #= S*S*S,
        NB #= sum([X[I]*10**(N-I) : I in 1..N]),
        S #= sum(X),

        labeling(X).