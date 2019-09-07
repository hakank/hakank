/*

  Ackermann function in SWI Prolog



  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

:- table ack/3.
go :-
        M = 3,
        between(0,10,N),
        writeln(n=N),
        abolish_all_tables,
        time(once(ack(M,N,R))),
        writeln([3,N,R]),
        fail,
        nl.

go.

ack(0,N,R) :-
        R #= N+1.
ack(M,0,R) :-
        M #> 0,
        M1 #= M -1, ack(M1,1,R).
ack(M,N,R) :-
        M #> 0, N #> 0,
        M1 #= M-1, N1 #= N-1,
        ack(M,N1,R1),
        ack(M1,R1,R).
