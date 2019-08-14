/*

  Number of days problem (knapsack) in SWI Prolog

  From Nathan Brixius
  "Solving a Knapsack problem with Solver Foundation and LINQ"
  http://blogs.msdn.com/natbr/archive/2010/05/06/solving-a-knapsack-problem-with-solver-foundation-and-linq.aspx
 """
  Let's say I have this list of days and prices:

    List<ReservationPrice> prices = new List<ReservationPrice>(); 
    prices.Add(new ReservationPrice { NumberOfDays = 1, Price = 1000 }); 
    prices.Add(new ReservationPrice { NumberOfDays = 2, Price = 1200 }); 
    prices.Add(new ReservationPrice { NumberOfDays = 3, Price = 2500 }); 
    prices.Add(new ReservationPrice { NumberOfDays = 4, Price = 3100 }); 
    prices.Add(new ReservationPrice { NumberOfDays = 7, Price = 4000 }); 

  What I would like to able to do now is: give me the best price 
  from the list based on a number of days.

  So if ask for 3 days the best price from the list is from child one 
  (1000) and two (1200), but there are of course different combinations. 
  How would an algorithm that found the best price from this list 
  look like ?
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-

        data(Days,Costs),
        writeln(days=Days),
        writeln(costs=Costs),
  
        sum(Days,#=,MaxDays),
        writeln(maxDays=MaxDays),

        %% Calculate the best deal for all days from 1..17
        findall([D,X,TotalCost],
                (between(1,MaxDays,D),
                 once(number_of_days(Days,Costs,D,X,TotalCost))
                ),
                L),
        maplist(writeln,L),
        nl.

number_of_days(Days,Costs,NumDays,X,TotalCost) :-
        sum(Days,#=,MaxDays),
        sum(Costs,#=,MaxCost),
        Days ins 1..MaxDays,
        TotalCost in 1..MaxCost,
        length(Days,Len),
        
        length(X,Len),
        X ins 0..1,
        scalar_product(Days,X,#=,NumDays),
        scalar_product(Costs,X,#=, TotalCost),
        
        labeling([min(TotalCost)], X).
         

data(Days,Cost) :- 
        Days = [1,2,3,4,7],
        Cost = [1000,1200,2500,3100,4000].
