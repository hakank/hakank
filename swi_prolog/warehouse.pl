/*

  Warehouse location problem in SWI Prolog

  From OPL model warehouse.mod
  Solution:
  """
  Optimal solution found with objective: 383
  open= [1 1 1 0 1]
  storesof= [{3} {1 5 6 8} {7 9} {} {0 2 4}]
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-

        Warehouses = ["Bonn", "Bordeaux", "London", "Paris", "Rome"],
        Capacity= [1,4,2,1,3],
        SupplyCost = 
        [[ 20, 24, 11, 25, 30 ], 
         [ 28, 27, 82, 83, 74 ],
         [ 74, 97, 71, 96, 70 ],
         [  2, 55, 73, 69, 61 ],
         [ 46, 96, 59, 83,  4 ],
         [ 42, 22, 29, 67, 59 ],
         [  1,  5, 73, 59, 56 ],
         [ 10, 73, 13, 43, 96 ],
         [ 93, 35, 63, 85, 46 ],
         [ 47, 65, 55, 71, 95 ]],
        
        Fixed = 30,
        
        length(SupplyCost,NumStores),
        transpose(SupplyCost,SupplyCostT),
        length(SupplyCostT,NumWarehouses),
        
        flatten(SupplyCost,SupplyCostList),
        
        %% suppliers
        new_matrix(NumStores, NumWarehouses, 0..1, Supply),
        flatten(Supply,SupplyList),
        
        %% Open suppliers
        length(Open,NumWarehouses),
        Open ins 0..1,

        %% Supply: only one warehouse for a supply
        maplist(sum_supply,Supply),

        maplist(supply_open(Open),Supply),
        
        %% check capacity
        transpose(Supply,SupplyT),
        maplist(check_capacity,SupplyT,Capacity),  

        
        %% calculate total costs: Fixed costs for the open
        total_cost(Open,Fixed,0,Costs1),
        
        %% cost for open suppliers
        scalar_product(SupplyCostList,SupplyList,#=,Costs2),

        TotalCosts #= Costs1 + Costs2,

        %% search
        flatten([SupplyList,Open], Vars),
        labeling([ff,enum,min(TotalCosts)], Vars),

        %% output
        writeln(open=Open),
        writeln("supply:"),
        maplist(writeln,Supply),
        nl,
        writeln(total_costs=TotalCosts),

        findall([WH,OpenW,Stores],
                (between(1,NumWarehouses,W),
                 nth1(W,Warehouses,WH),
                 nth1(W,Open,OpenW),
                 findall(S,
                         (between(1,NumStores,S),
                          matrix_element(Supply,S,W,1)
                         ),
                         Stores)
                 ),
                Sol),
        maplist(format("~w: ~w Stores: ~w~n"),Sol),
        nl.

%% Supply: only one warehouse for a supply
sum_supply(SRow) :-
        sum(SRow,#=,1).

%%
%% A supply that supply something must be open
%%
supply_open(Open,Supply) :-
        maplist(supply_open_1,Open,Supply).
supply_open_1(O,S) :-
        S #=< O.

%%
%% check capacity
%%
check_capacity(SupplyCol,Cap) :-
        sum(SupplyCol, #=<, Cap).

%%
%% calculate total costs: Fixed costs for an open warehouse.
%%
total_cost([],_Fixed,TotalCost,TotalCost).
total_cost([O|Os],Fixed,TotalCost0,TotalCost) :-
        TotalCost1 #= TotalCost0 + Fixed * O,
        total_cost(Os,Fixed,TotalCost1,TotalCost).