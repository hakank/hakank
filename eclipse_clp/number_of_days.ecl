/*

  Number of days problem (knapsack) in ECLiPSe.

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

  Compare with the MiniZinc model:
  http://www.hakank.org/minizinc/number_of_days.mzn

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

/*
   Solution (edited)

     num_days : 1
     x : [1, 0, 0, 0, 0]
     total_cost : 1000
     
     num_days : 2
     x : [0, 1, 0, 0, 0]
     total_cost : 1200
     
     num_days : 3
     x : [1, 1, 0, 0, 0]
     total_cost : 2200
     
     num_days : 4
     x : [0, 0, 0, 1, 0]
     total_cost : 3100
     
     num_days : 5
     x : [0, 1, 1, 0, 0]
     total_cost : 3700
     
     num_days : 6
     x : [0, 1, 0, 1, 0]
     total_cost : 4300
     
     num_days : 7
     x : [0, 0, 0, 0, 1]
     total_cost : 4000
     
     num_days : 8
     x : [1, 0, 0, 0, 1]
     total_cost : 5000
     
     num_days : 9
     x : [0, 1, 0, 0, 1]
     total_cost : 5200
     
     num_days : 10
     x : [1, 1, 0, 0, 1]
     total_cost : 6200
     
     num_days : 11
     x : [0, 0, 0, 1, 1]
     total_cost : 7100
     
     num_days : 12
     x : [0, 1, 1, 0, 1]
     total_cost : 7700
     
     num_days : 13
     x : [0, 1, 0, 1, 1]
     total_cost : 8300
     
     num_days : 14
     x : [1, 1, 0, 1, 1]
     total_cost : 9300
     
     num_days : 15
     x : [1, 0, 1, 1, 1]
     total_cost : 10600
     
     num_days : 16
     x : [0, 1, 1, 1, 1]
     total_cost : 10800
     
     num_days : 17
     x : [1, 1, 1, 1, 1]
     total_cost : 11800

*/


:-lib(ic).
:-lib(listut).
:-lib(branch_and_bound).


go :-
        data(Data),
        ( foreach([Day,Cost], Data),
          foreach(Day,Days),
          foreach(Cost,Costs) do
              true
        ),
  
        sum(Days,MaxDays),

        % Calculate the best deal for all days from 1..17
        (for(D,1,MaxDays),
         param(Days,Costs) do
             number_of_days(Days,Costs,D,X,TotalCost),
             writeln(num_days:D),
             writeln(x:X),
             writeln(total_cost:TotalCost),nl
        ).
             


number_of_days(Days,Costs,NumDays,X,TotalCost) :-

        MaxDays = sum(Days),
        MaxCost = sum(Costs),
        Days :: 1..MaxDays,
        TotalCost :: 1..MaxCost,

        length(Days,Len),
        length(X,Len),
        X :: 0..1,
        NumDays #= X*Days,
        TotalCost #= X * Costs,
        minimize(labeling(X),TotalCost).
                   

data([[1,1000],
      [2,1200],
      [3,2500],
      [4,3100],
      [7,4000]]).

% Number of days, Cost
% days([1,2,3,4,7]).
% cost([1000,1200,2500,3100,4000]).