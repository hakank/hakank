/*

  Smuggler's knapsack problem  in SWI Prolog

  
  Marriott & Stuker: 'Programming with constraints', page  101f, 115f

  Smuggler's knapsack.
  
  A smuggler has a knapsack with a capacity of 9 units.
              Unit       Profit
  Whisky:     4 units    15 dollars
  Perfume:    3 units    10 dollars
  Cigarettes: 2 units     7 dollars

  What is the optimal choice?

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go:-
        Capacity = 9,
        once(smuggler([W,P,C,Profit,Capacity],Capacity)),
        writeln([whisky=W,perfume=P,cigarettes=C,profit=Profit,capacity=Capacity]).

%%
%% Different capacities of the knapsack
%%
go2:-
        %% member(Capacity,[2,3,5,7,11,13,17,23,29,31]),
        between(1,31,Capacity),
        once(smuggler([W,P,C,Profit,Capacity],Capacity)),
        write([whiskey=W,perfume=P,cigarettes=C,profit=Profit,capacity=Capacity]),nl,
        fail.
go2.


smuggler([W,P,C,Profit,Capacity],Capacity) :-

        [W,P,C] ins 0..9,

        %                 Unit       Profit
        % Whisky:     4 units    15 dollars
        % Perfume:    3 units    10 dollars
        % Cigarettes: 2 units     7 dollars

        % Units
        4*W  + 3*P  + 2*C #=< Capacity,

        % Profit.
        15*W + 10*P + 7*C #= Profit,
        
        labeling([max(Profit)],[W,P,C,Profit,Capacity]).
