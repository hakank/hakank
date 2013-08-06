/*

  Warehouse location problem in B-Prolog.

  From OPL model warehouse.mod
  Solution:
  """
  Optimal solution found with objective: 383
  open= [1 1 1 0 1]
  storesof= [{3} {1 5 6 8} {7 9} {} {0 2 4}]
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


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
        flatten(SupplyCost,SupplyCostList),

        % suppliers
        matrix(Supply,[NumStores, NumWarehouses]),
        flatten(Supply,SupplyList),
        SupplyList :: 0..1,


        % Open suppliers
        length(Open,NumWarehouses),
        Open :: 0..1,

        % Supply
        foreach(SS in Supply, sum(SS,#=,1)),
        foreach(SS in Supply,
                foreach((O,S) in (Open,SS), S #=< O)
        ),


        % check capacity
        transpose(Supply,SupplyTransposed),
        foreach((Column,Cap) in (SupplyTransposed, Capacity), sum(Column,#=<,Cap)),

        
        % calculate total costs: Fixed costs for the open
        Costs1 #= sum([Fixed*O :  O in Open]),

        % cost for open suppliers
        scalar_product(SupplyCostList,SupplyList,#=,Costs2),

        TotalCosts #= Costs1 + Costs2,

        % search
        term_variables([SupplyList,Open],Vars1),
        labeling([ff,minimize(TotalCosts)], Vars1),


        % output
        writeln(open:Open),
        writeln('supply:'),
        foreach(S in Supply, writeln(S)),
        nl,
        foreach((Warehouse,W) in (Warehouses,1..NumWarehouses),
                [OpenW],
                (
                    element(W,Open,OpenW),
                    format('~w: open: ~w  Stores: ', [Warehouse, OpenW]),
                    foreach(S in 1..NumStores,
                            [SupplySW],
                            (
                                matrix_element(Supply,S,W,SupplySW),
                                SupplySW =:= 1  -> 
                                    format('~w ', [S])
                            ; 
                                    true
                            )
                           ),nl
                )
               ),

        writeln(total_costs:TotalCosts),
        nl.

transpose(Matrix,Transposed):-
    N is Matrix^length,
    M is Matrix[1]^length,
    Transposed @= [MJI : J in 1..M, [MJI],
                (MJI @= [Matrix[I,J] : I in 1..N])].


matrix_element(X, I, J, Val) :-
        nth1(I, X, Row),
        element(J, Row, Val).



matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        foreach(X in L, matrix(X, Dims)).
