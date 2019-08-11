/*

  P-median problem in SWI Prolog

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
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        data(1,Customers,Warehouses,Demand,Distance,P),
        print_p_median(Customers,Warehouses,Demand,Distance,P),
        nl.

%%
%% Test all P in 1..3
%%
go2 :-
        data(1,Customers,Warehouses,Demand,Distance,_P),
        between(1,3,P),
        once(print_p_median(Customers,Warehouses,Demand,Distance,P)),
        nl,
        fail,
        nl.
go2.


%%
%% Wrapper
%%
print_p_median(Customers,Warehouses,Demand,Distance,P) :-

        writeln(p=P),
        p_median(Customers,Warehouses,Demand,Distance,P, OpenWarehouse,ShipToCustomer,Z),

        length(Customers,NumCustomers),
        length(Warehouses,NumWarehouses),

        writeln(z=Z),

        %% Which warehouses are open?
        writeln(openWarehouse=OpenWarehouse),
        findall(OpenW,(between(1,NumWarehouses,W),
                       element(W,OpenWarehouse,1),
                       nth1(W,Warehouses,OpenW)
                       ),
                Open),
        maplist(format("Open: ~w~n"),Open),
        
        %% Which warehouse to which customer?
        findall([Cust,S,WW],
                (between(1,NumCustomers,C),
                 nth1(C,Customers,Cust),
                 nth1(C,ShipToCustomer,S),
                 findall(W,
                         (between(1,NumWarehouses,J),
                          element(J,S,1),
                          nth1(J,Warehouses,W)
                         ),
                         WW)
                ),
                CustSol),
        maplist(format("Customer ~w: ~w ~w~n"),CustSol),
        nl.
        

%%
%% Get the solution
%%
p_median(Customers,Warehouses,Demand,Distance,P, OpenWarehouse,ShipToCustomer,Z) :-

        length(Customers,NumCustomers),
        length(Warehouses,NumWarehouses),
               
        %% decision variables
        length(OpenWarehouse,NumWarehouses),
        OpenWarehouse ins 0..1,
        
        new_matrix(NumCustomers,NumWarehouses,0..1,ShipToCustomer),
        
        maplist(sumz,Demand,Distance,ShipToCustomer,Zs),
        sum(Zs,#=,Z),
        
        maplist(sum_ship_to_customer,ShipToCustomer),
        
        sum(OpenWarehouse,#=,P),

        transpose(ShipToCustomer,ShipToCustomerT),
        maplist(check_open,ShipToCustomerT,OpenWarehouse),
        
        flatten([OpenWarehouse,ShipToCustomer],Vars),
        labeling([min(Z)],Vars).


sumz(Demand,Distance,ShipToCustomer, Zs) :-
        scalar_product(Distance,ShipToCustomer,#=,T1),
        Zs #= Demand*T1.

sum_ship_to_customer(ShipToCustomer) :-
        sum(ShipToCustomer,#=,1).

check_open(ShipToCustomer,OpenWarehouse) :-
        maplist(check_open_(OpenWarehouse),ShipToCustomer).

check_open_(O,S) :-
        S #=< O.


data(1,Customers,Warehouses,Demand,Distance,P) :-
        Customers = ["Albert","Bob","Chris","Daniel"],
        Warehouses = ["Santa Clara", "San Jose", "Berkeley"],
        Demand = [100,80,80,70],
        Distance = [[2, 10, 50],
                    [2, 10, 52],
                    [50, 60,  3],
                    [40, 60,  1]],
        P = 2.
