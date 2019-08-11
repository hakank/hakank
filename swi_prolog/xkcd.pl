/*

  xkcd's knapsack/subset-sum problem in SWI Prolog

  http://xkcd.com/287/

  Some amount (or none) of each dish should be ordered to give a total of exact 15.05


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        findall(X,xkcd(X),L),
        writeln(L).

xkcd(X) :-
        Prices = [215, 275, 335, 355, 420, 580],
        Total = 1505,
        length(Prices,Len),
        length(X,Len),
        X ins 0..10,
        scalar_product(Prices, X, #=, Total),
        label(X).
