/*

  Pair divides the sum puzzle in B-Prolog.

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

  (This was ported from my MiniZinc model:
   http://www.hakank.org/minizinc/pair_divides_the_sum.mzn)

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-
        findall(_,problem,_).

% There are no solutions for 8 or 9 (for MaxVal=100)
problem :-
        N :: 6..7,
        % N = 7,
        indomain(N),
        writeln(n:N),

        MaxVal = 100,
        length(X, N),
        X :: 1..MaxVal,
        Z :: N..MaxVal*N,

        alldifferent(X),
        increasing(X),

        Z #= sum(X),
        Z mod N #= 0,
        foreach(I in 1..N, J in I+1..N, Z mod abs(X[I]-X[J]) #= 0),

        term_variables([X], Vars),
        labeling([constr,reverse_split], Vars),

        writeln(x:X),
        writeln(z:Z),

        nl.
                
        
increasing(List) :-
        foreach(I in 2..List^length, List[I-1] #=< List[I]).
