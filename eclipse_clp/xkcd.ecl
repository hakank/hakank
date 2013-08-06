/*

  xkcd Knapsack problem in ECLiPSe.

  http://xkcd.com/287/

  Some amount (or none) of each dish should be ordered to give a total
  of exact 15.05.

  The prices of the dishes are: 
   215, 275, 335, 355, 420, 580


  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/xkcd.mzn
  * Comet   : http://www.hakank.org/comet/xkcd.co
  * Gecode/R: http://www.hakank.org/gecode_r/xkcd.rb
  * Gecode  : http://www.hakank.org/gecode/xkcd.cpp


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/


*/

:- lib(ic).
:- lib(ic_global).
:- lib(ic_search).


% 
% Sum the totals: Total = Prices x Xs
%
knapsack(Prices, Total, Xs) :-
        dim(Prices, [Len]), % get the length of the array
        ( for(I, 1, Len), fromto(0, S1, S2, Sum), param(Prices, Xs) do
              S2 = S1+Prices[I]*Xs[I]
        ),
        Total #= eval(Sum),

        labeling(Xs).


%
% simple version: Total = 1505
%
% This use the array representation
%
go :-
        Prices = [](215, 275, 335, 355, 420, 580), % Prices for the dishes
        Total = 1505,       % multiply 15.05 with 100 for integer usage
        dim(Prices, [Len]), % get length of Prices
        dim(Xs, [Len]),     % same length of Xs 
        Xs :: [0..10],      % domain of Xs
        findall(Xs, knapsack(Prices, Total, Xs), L),
        length(L, LLen),    % length of solutions
        writeln([total:Total, num_solutions:LLen, solutions:L]).

%
% extended version: 
% show all combinations for Total of 1400..1510
%
go2 :-
        % note: Prices is an array (not a list)
        Prices = [](215, 275, 335, 355, 420, 580), % Prices for the dishes
        % Total = 1505,
        dim(Prices, [Len]), % get length of Prices
        dim(Xs, [Len]),     % same length of Xs 
        Xs :: [0..10],
        ( for(Total, 1400, 1510), param(Prices,Xs) do 
            findall(Xs, knapsack(Prices, Total, Xs), L),
            length(L, LLen),
            % show only if at least one solution
            LLen > 0 -> writeln([Total, LLen, L]); true
        ).


%
% Minimize the number of dishes.
%
go3 :-
        Prices = [](215, 275, 335, 355, 420, 580), % Prices for the dishes
        Total = 1505,       % multiply 15.05 with 100 for integer usage
        dim(Prices, [Len]), % get length of Prices
        dim(Xs, [Len]),     % same length of Xs 
        Xs :: [0..10],      % domain of Xs
        % get all solutions
        knapsack(Prices, Total, Xs),

        collection_to_list(Xs, XsList),
        sum(XsList, NumDishes),
        minimize(search(XsList,0,first_fail,indomain_max,complete,[]),NumDishes),
        writeln([Total, Xs, NumDishes]).



% 
% Simple list version.
%
go4 :-
        Prices = [215, 275, 335, 355, 420, 580],
        Total = 1505,
        length(Prices, Len),
        length(Xs, Len),
        Xs :: [0..10],
        Total #= Xs * Prices,
        labeling(Xs),
        writeln([Total, Xs]).
