/*

  Xkcd knapsack/subset sum problem in B-Prolog.

  http://xkcd.com/287/

  Some amount (or none) of each dish should be ordered to give a total
  of exact 15.05.

  The prices of the dishes are: 
   215, 275, 335, 355, 420, 580


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

% 
% Sum the totals: Total = Prices x Xs
%
knapsack(Prices, Total, Xs) :-
        Total #= sum([S2 : I in 1..Prices^length,
                      [S2],
                      S2 #= Prices[I]*Xs[I]
                     ]),
        labeling(Xs).


%
% simple version: Total = 1505
%
go :-
        Prices = [215, 275, 335, 355, 420, 580], % Prices for the dishes
        Total = 1505,       % multiply 15.05 with 100 for integer usage
        Len @= Prices^length,
        length(X,Len),
        X :: 0..10,      % domain of Xs
        findall(X, knapsack(Prices, Total, X), L),
        length(L, LLen),    % number of solutions
        writeln([total:Total, num_solutions:LLen, solutions:L]).

%
% extended version: 
% show all combinations for Total of 1400..1510
%
go2 :-
        Prices = [215, 275, 335, 355, 420, 580], % Prices for the dishes
        Len @= Prices^length,
        N = 10,
        foreach(Total in 1400..1520,
                [X,L,LLen],
                (
                    length(X,Len),
                    X :: 0..N,      % domain of Xs
                    findall(X, knapsack(Prices, Total, X), L),
                    length(L, LLen),    % number of solutions
                    LLen > 0 ->
                        writeln([total:Total, num_solutions:LLen,
                                 solutions:L])
                ;
                        true
                )
               ).


%
% Minimize the number of dishes.
%
go3 :-
        Prices = [215, 275, 335, 355, 420, 580], % Prices for the dishes
        Total = 1505,       % multiply 15.05 with 100 for integer usage
        length(Prices, Len), % get length of Prices
        length(Xs, Len),     % same length of Xs 
        Xs :: [0..10],      % domain of Xs

        knapsack(Prices, Total, Xs),
        NumDishes #= sum(Xs), % number of dishes
        minof(labeling([ff], Xs),NumDishes),
        format('Total: ~d Xs: ~q NumDishes: ~d\n', [Total, Xs,NumDishes]).



% % 
% % Simple list version.
% %
go4 :-
        Prices = [215, 275, 335, 355, 420, 580],
        Total = 1505,
        length(Prices, Len),
        length(Xs, Len),
        Xs :: [0..10],
        scalar_product(Prices,Xs, #=, Total ),
        labeling(Xs),
        writeln([Total, Xs]).
