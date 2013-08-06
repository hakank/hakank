/*

  Bus scheduling in SICStus Prolog.


  Problem from Taha "Introduction to Operations Research", page 58.
  Scheduling of buses during a day.

  This is a slightly more general model than Taha's.

  Compare with the following models:
  * MiniZinc:  http://www.hakank.org/minizinc/bus_scheduling.mzn
  * Comet   :  http://www.hakank.org/comet/bus_schedule.co
  * ECLiPSe :  http://www.hakank.org/eclipse/bus_schedule.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


go :-
        % number of time slots
        Time_slots = 6,

        % demand: minimum number of buses at time t
        Demands = [8, 10, 7, 12, 4, 4],

        sumlist(Demands, Max_num),
        write([max_num:Max_num]), nl,
        
        % result: how many buses start the schedule at time slot t?
        length(X, Time_slots),
        domain(X, 0, Max_num),

        % the objective to minimize: the total number of buses
        sum(X, #=, Num_buses),
        write(after_sum), nl,

        % meet the demands for this and the next time slot
        ( 
            for(I, 1,Time_slots-1),
            param(X, Demands)
        do
            nth1(I,X,XI),
            I1 is I+1,
            nth1(I1,X,XI1),
            nth1(I1,Demands,DI),
            XI+XI1 #>= DI
        ),

        % demand "around the clock"
        element(Time_slots,X,LastX),
        element(1,X,FirstX),
        element(Time_slots, X, LastD),
        FirstX + LastX #>= LastD,

        % For all solutions
        % Num_buses #= 26,
        % labeling([ff],X),

        % for minimizing
        labeling([ff,minimize(Num_buses)],X),
        write([Num_buses, X]),nl,
        fd_statistics.