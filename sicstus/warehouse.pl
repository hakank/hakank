/*

  Warehouse location problem in SICStus Prolog.

  From OPL model warehouse.mod
  Solution:
  """
  Optimal solution found with objective: 383
  open= [1 1 1 0 1]
  storesof= [{3} {1 5 6 8} {7 9} {} {0 2 4}]
  """

  Compare with the following models:
  * Comet  : http://www.hakank.org/comet/warehouse.co
  * ECLiPSe: http://www.hakank.org/eclipse/warehouse.ecl

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).



go :-

        Warehouses = ['Bonn', 'Bordeaux', 'London', 'Paris', 'Rome'],
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

        matrix(SupplyCost,[NumStores,NumWarehouses]),
        append(SupplyCost,SupplyCostList),

        % suppliers
        matrix(Supply,[NumStores, NumWarehouses]),
        append(Supply,SupplyList),
        domain(SupplyList,0,1),

        % Open suppliers
        length(Open,NumWarehouses),
        domain(Open,0,1),

        % Supply
        ( foreach(S,Supply)
        do
          sum(S,#=,1)
        ),

        ( foreach(Supply, Supply),
          param(Open) do
              ( foreach(O, Open),
                foreach(S, Supply) do
                    S #=< O
              )
        ),

        % check capacity
        transpose(Supply,SupplyTransposed),
        ( foreach(Column,SupplyTransposed),
          foreach(Cap,Capacity) do
              sum(Column,#=<,Cap)
        ),
        
        % calculate total costs: Fixed costs for the open
        ( foreach(O,Open),
          fromto(0,In1,Out1,Costs1),
          param(Fixed) do
              Out1 #= In1 + Fixed*O
        ),

        % cost for open suppliers
        scalar_product(SupplyCostList,SupplyList,#=,Costs2),

        TotalCosts #= Costs1 + Costs2,

        % search
        append(SupplyList,Open,Vars1),
        % append(Vars1,[TotalCosts], Vars),
        labeling([ff,step,up,minimize(TotalCosts)], Vars1),


        % output
        write(open:Open),nl,
        write('supply:'),nl,
        (foreach(S,Supply) do 
             write(S),nl
        ),
        nl,
        ( foreach(Warehouse,Warehouses),
          for(W,1,NumWarehouses), 
          param(Open,Supply,NumStores) do
              element(W,Open,OpenW),
              format('~w: open: ~w  Stores: ', [Warehouse, OpenW]),
              ( for(S,1,NumStores),
                param(Supply,W) do
                    matrix_element(Supply,S,W,SupplySW),
                    SupplySW =:= 1 
              -> 
                format('~w ', [S])
              ; 
                true
              ),
              nl
        ),

        write(total_costs:TotalCosts),nl,nl,
        fd_statistics.


matrix_element(X, I, J, Val) :-
        nth1(I, X, Row),
        element(J, Row, Val).


% From Mats Carlsson.
matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).
