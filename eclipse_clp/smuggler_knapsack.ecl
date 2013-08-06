/*

  Smuggler's knapsack problem in ECLiPSe.

  Marriott & Stuker: 'Programming with constraints', page  101f, 115f

  Smuggler's knapsack.
  
  A smuggler has a knapsack with a capacity of 9 units.
              Unit       Profit
  Whisky:     4 units    15 dollars
  Perfume:    3 units    10 dollars
  Cigarettes: 2 units     7 dollars

  What is the optimal choice?


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/


:- lib(ic), lib(ic_search),lib(branch_and_bound).


go:-
        findall([W,P,C,Profit,Constraint],smuggler([W,P,C,Profit,Constraint],9),L),
        writeln(L).

go2:-
        member(X,[2,3,5,7,11,13,17,23,29,31]),
        smuggler(LL,X),
        write(LL),nl,
        fail.


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

        NegProfit #= -Profit,
        
        % minimize(labeling([W,P,C,Profit,Constraint]),NegProfit).
        minimize(
                    search([W,P,C,Profit,Constraint],
                           0,smallest,indomain_max,complete,[]),
                    NegProfit).
