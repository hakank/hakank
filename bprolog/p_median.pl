/*

  P-median problem in B-Prolog.

  Model and data from the OPL Manual, which describes the problem:
  """
  The P-Median problem is a well known problem in Operations Research. 
  The problem can be stated very simply, like this: given a set of customers 
  with known amounts of demand, a set of candidate locations for warehouses, 
  and the distance between each pair of customer-warehouse, choose P 
  warehouses to open that minimize the demand-weighted distance of serving 
  all customers from those P warehouses.
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/


go :-
        P = 2,
        Customers = ['Albert','Bob','Chris','Daniel'],
        length(Customers,NumCustomers),
        Warehouses = ['Santa Clara', 'San Jose', 'Berkeley'],
        length(Warehouses,NumWarehouses),
        writeln([numCustomers:NumCustomers,numWarehouses:NumWarehouses]),
        Demand = [100,80,80,70],
        Distance = 
        [[2, 10, 50],
         [2, 10, 52],
         [50, 60,  3],
         [40, 60,  1]],

        
        % decision variables
        length(OpenWarehouse, NumWarehouses),
        OpenWarehouse :: 0..1,
        
        new_array(ShipToCustomer, [NumCustomers,NumWarehouses]),
        array_to_list(ShipToCustomer,ShipToCustomerVars),
        ShipToCustomerVars :: 0..1,

        Z $= sum([Demand[C]*Distance[C,W]*ShipToCustomer[C,W]  :
                C in 1..NumCustomers, W in 1..NumWarehouses]),

        
        foreach(C in 1..NumCustomers,
                sum([ ShipToCustomer[C,W] : W in 1..NumWarehouses]) $= 1
                ),

        sum(OpenWarehouse) $= P,

        foreach(C in 1..NumCustomers, W in 1..NumWarehouses,
                ShipToCustomer[C,W] $=< OpenWarehouse[W]),


        term_variables([OpenWarehouse,ShipToCustomerVars],Vars),

        % ip_solve([min(Z)],Vars),
        cp_solve([min(Z)],Vars),

        writeln(z:Z),
        writeln(openWarehouse:OpenWarehouse),
        foreach(W in 1..NumWarehouses,
                [WW],
                (
                    OpenWarehouse[W] =:= 1 ->
                        WW @= Warehouses[W],
                        writeln(open:WW)
                ;
                        true
                )),
        
        foreach(C in 1..NumCustomers,    
                [S,Cust,WW],
                (S @= [ShipToCustomer[C,W] : W in 1..NumWarehouses],
                 Cust @= Customers[C],
                 WW @= [Warehouses[W]: W in 1..NumWarehouses, S[W] =:= 1],
                 format("Customer ~w: ~w ~w\n",[Cust,S,WW] )
                )
               ),

        nl.

        
        
