/*

  Xkcd problem (knapsack) in SICStus Prolog.

  http://xkcd.com/287/

  Some amount (or none) of each dish should be ordered to give a total 
  of exact 15.05

  Compare with the following models:
  * Comet: http://www.hakank.org/comet/xkcd.co
  * ECLiPSE: http://www.hakank.org/eclipse/xkcd.ecl
  * Gecode: http://www.hakank.org/gecode/xkcd.cpp
  * Gecode/R: http://www.hakank.org/gecode_r/xkcd.rb
  * MiniZinc: http://www.hakank.org/minizinc/xkcd.mzn
  * Tailor: http://www.hakank.org/minizinc/xkcd.mzn 

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


% 
% Sum the totals: Total = Coeffs x Xs
%
my_knapsack(Coeffs, Xs, Total) :-
        ( foreach(C, Coeffs),
          foreach(X, Xs),
            fromto(0, S1, S2, Sum)
        do
        S2 #= S1+C*X
        ),
        Total #= Sum.

%
% A faster variant
%
fast_knapsack(Coeffs, Xs, Total) :-
        scalar_product(Coeffs, Xs, #=, Total,[consistency(domain)]).


go :-
        Price = [215, 275, 335, 355, 420, 580],
        length(Price, Len),
        length(Xs, Len),
        domain(Xs, 0, 100),
        Total = 1505,  % multiply by 100 to be able to use integers
        findall(Xs,(
                    % 39 backtracks:
                    my_knapsack(Price, Xs, Total),
                    % 0 backtracks;
                    % fast_knapsack(Price, Xs, Total),
                    labeling([ff],Xs)
               ), L),
        write(L), nl,
        fd_statistics,
        halt.

