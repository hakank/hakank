/*

  Simple money change problem in B-Prolog.

  Generate all possible ways to get Value from a set of coins.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

go :-
        problem(1,Value, Coins),
        solve(Value,Coins).

go2 :-
        problem(2,Value, Coins),
        solve(Value,Coins).

solve(Value,Coins) :-
        findall(X, money(Value, Coins, X), L),
        writeln(L),
        length(L, Len),
        writeln(len:Len),
        nl.


money(Value, Coins, X) :-
        
        length(Coins,N),
        length(X, N),
        X :: 0..Value,
        scalar_product(Coins, X, #= , Value),
        labeling(X).


% problem(Problem, Value, Coins).
problem(1, 100, [100,50,25,10,5,1]).
problem(2, 100, [100,50,25,10,5,2,1]).

        