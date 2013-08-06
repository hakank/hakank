/*

  Smuggler's knapsack problem in B-Prolog.

  Marriott & Stucker: 'Programming with constraints', page  101f, 115f

  Smuggler's knapsack.
  
  A smuggler has a knapsack with a capacity of 9 units.
              Unit       Profit
  Whisky:     4 units    15 dollars
  Perfume:    3 units    10 dollars
  Cigarettes: 2 units     7 dollars

  What is the optimal choice?

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


go:-
        findall([W,P,C,Profit,Constraint],smuggler([W,P,C,Profit,Constraint],9),L),
        writeln(L),nl.

go2:-
        List = [2,3,5,7,11,13,17,23,29,31],
        foreach(L in List, [X],
                (smuggler(X,L), writeln(X))).



smuggler([W,P,C,Profit,Constraint],Constraint) :-

        [W,P,C] :: 0..9,

        %                 Unit       Profit
        % Whisky:     4 units    15 dollars
        % Perfume:    3 units    10 dollars
        % Cigarettes: 2 units     7 dollars

        % Units
        4*W  + 3*P  + 2*C #=< Constraint,

        % Profit.
        15*W + 10*P + 7*C #= Profit,
       
        labeling([maximize(Profit)], [W,P,C,Profit,Constraint]).

