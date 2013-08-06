/*

  Golomb ruler in B-Prolog.

  A Golomb ruler is a set of integers (marks) a(1) < ...  < a(n) such
  that all the differences a(i)-a(j) (i > j) are distinct.  Clearly we
  may assume a(1)=0.  Then a(n) is the length of the Golomb ruler.
  For a given number of marks, n, we are interested in finding the
  shortest Golomb rulers.  Such rulers are called optimal. 

  See http://www.research.ibm.com/people/s/shearer/grule.html

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-
        time2(golomb(8, Xs)),
        writeln(Xs).


time2(Goal):-
        cputime(Start),
        statistics(backtracks, Backtracks1),
        call(Goal),
        statistics(backtracks, Backtracks2),
        cputime(End),
        T is (End-Start)/1000,
        Backtracks is Backtracks2 - Backtracks1,
        format('CPU time ~w seconds. Backtracks: ~d\n', [T, Backtracks]).


golomb(N, Xs) :-

        writeln('N':N),
        length(Xs, N),% model
        NN is 2**(N-1)-1,
        Xs :: 0..NN,
        Xn #= Xs[N], % to minimize

        alldifferent(Xs),
        increasing(Xs),
        Xs[1] #= 0,

        Diffs @= [Diff : I in 1..N, J in 1..N,  [Diff], 
                  (I \= J, Diff #= Xs[I]-Xs[J])],
        alldifferent(Diffs),

        % Symmetry breaking
        Diffs[1] #< Diffs[N],
        Xs[2] - Xs[1] #< Xs[N] - Xs[N-1],

        term_variables([Diffs,Xs], Vars),
        minof(labeling([ff,down],Vars), Xn, format("Xn: ~d\n",[Xn])).


increasing(List) :-
        Len @= List^length,
        foreach(I in 2..Len, List[I-1] #< List[I]).

