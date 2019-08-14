/*

  Simple knapsack problem in SWI Prolog

  Problem from http://sourceforge.net/forum/forum.php?thread_id=1432186&forum_id=335511
  """
  Knapsack maximization problem example
  @author Fernando Lopez Hernandez (f.lopezATuamDOTes)

  In this problem a thief have a knapsack with capacity of 10 units.
  He could charge the knapsack with golden ingots of size 4, silver ingots
  of size 3, and bronze ingots of size 2. Each ingot value is 15, 12 and 7
  respectively.

  The solver goal is to find a solution who maximize profit with the above
  restrictions. That is to say: If G represents the number of golden ingots,
  S the number of silver ingots, B the number of bronze ingots,
  and P the profit, we define the following constraints:
  4G + 3S + 2B <= 10
  15G + 12S + 7B = P

  Solution:
  [ 1, 2, 0 ]
  i.e. 1 Gold, 2 Silver and 0 Bronze
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        values(Values),
        weights(Weights),
        weight_max(WeightMax),
        
        length(Values,N),

        %% decision variables
        length(X,N),
        X ins 0..100,

        knapsack(Weights, Values, X, WeightMax,Profit),

        labeling([max(Profit)], X),

        writeln(x=X),
        writeln(profit=Profit),
  
        nl.

% data
weight_max(10).

% Gold, Silver, Bronze        
values([15, 12, 7]).
weights([4, 3, 2]).

%%
%% knapsack: 
%%  given Weights and Values
%%  - ensure that the total weights <= WeightMax
%%  - calculate the profit (which will be maximized)
%%
knapsack(Weights, Values,Take, WeightMax,Profit) :-
        scalar_product(Weights,Take,#=<,WeightMax),
        scalar_product(Values,Take,#=,Profit).