/*

  Warehouse location problem in ECLiPSe.

  From OPL model warehouse.mod
  Solution:
  """
  Optimal solution found with objective: 383
  open= [1 1 1 0 1]
  storesof= [{3} {1 5 6 8} {7 9} {} {0 2 4}]
  """

  Compare with the Comet model:
  http://www.hakank.org/comet/warehouse.co


  (Also, compare with the Warehouse location example from the ECLiPSe site:
  http://eclipse-clp.org/examples/warehouse.ecl.txt
  The (larger) data for that examples is from P. Van Hentenryck: 
  Constraint Satisfaction in Logic Programming)

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
%:-lib(ic_global).
%:-lib(ic_search).
:-lib(branch_and_bound).
%:-lib(listut).
% :-lib(propia).


go :-

        Warehouses = []("Bonn", "Bordeaux", "London", "Paris", "Rome"),
        Capacity= [](1,4,2,1,3),
        SupplyCost = 
        []([]( 20, 24, 11, 25, 30 ), 
           []( 28, 27, 82, 83, 74 ),
           []( 74, 97, 71, 96, 70 ),
           [](  2, 55, 73, 69, 61 ),
           []( 46, 96, 59, 83,  4 ),
           []( 42, 22, 29, 67, 59 ),
           [](  1,  5, 73, 59, 56 ),
           []( 10, 73, 13, 43, 96 ),
           []( 93, 35, 63, 85, 46 ),
           []( 47, 65, 55, 71, 95 )),

        Fixed = 30,

        dim(SupplyCost,[NumStores,NumWarehouses]),

        dim(Open,[NumWarehouses]),
        Open :: 0..1,

        dim(Supply,[NumStores, NumWarehouses]),
        Supply :: 0..1,


        ( for(S,1,NumStores), param(Supply,NumWarehouses) do
              SupplyS is Supply[S,1..NumWarehouses],
              sum(SupplyS) #= 1
         ),


        (for(W,1,NumWarehouses), 
         param(Supply,Open,NumStores) do
             ( for(S,1,NumStores),param(Supply,Open,W) do
                   Supply[S, W] #=< Open[W]
             )
        ),

        ( for(W,1,NumWarehouses),
          param(Supply,Capacity,NumStores) do
              SupplySW is Supply[1..NumStores, W],
              sum(SupplySW) #=< Capacity[W]
        ),


        % minimize total costs
        ( for(W,1,NumWarehouses),
          fromto(0,In1,In1+Fixed*Open[W],Costs1),
          param(Open,Fixed) do
              true
        ),

        ( for(W,1,NumWarehouses) * for(S,1,NumStores),
          fromto(0,In1,In1 + SupplyCost[S,W]*Supply[S,W],Costs2),
          param(SupplyCost,Supply) do
              true
        ),

        TotalCosts #= eval(Costs1) + eval(Costs2),
        
        %
        % search
        %
        term_variables([Open,Supply,TotalCosts], Vars),
        minimize(search(Vars,0,first_fail,indomain_middle,complete,[backtrack(Backtracks)]),TotalCosts),

        %
        % print the warehouses and stores
        %
        ( foreacharg(Warehouse,Warehouses),
          for(W,1,NumWarehouses), 
          param(Open,Supply,NumStores) do
              OpenW is Open[W],
              printf("%w: open: %w  Stores: ", [Warehouse, OpenW]),
              
              ( for(S,1,NumStores),
                param(Supply,W) do
                    SupplySW is Supply[S,W],
                    SupplySW == 1 
              -> 
                printf("%w ", [S])
              ; 
                true
              ),
              nl
        ),

        writeln(total_costs:TotalCosts),
        writeln(backtracks:Backtracks).

