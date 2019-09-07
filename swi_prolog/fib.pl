/*

  Fibonacci in SWI Prolog

  This is a port of the B-Prolog program
  http://www.picat-lang.org/bprolog/examples/tabling/fib.pl
  (with some changes in go/0).

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:-table fib/2.

go :-
        findall(F,
                (between(0,100,N),
                 fib(N,F)
                ),
                Fs),
        writeln(Fs),
        nl.


fib(0, 1) :- !.
fib(1, 1) :- !.
fib(N,F) :-
        N > 1,
        N1 is N-1,
        N2 is N-2,
        fib(N1,F1),
        fib(N2,F2),
        F is F1+F2.
