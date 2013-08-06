/*

  Bus scheduling in ECLiPSe.

  Problem from Taha "Introduction to Operations Research", page 58.
  Scheduling of buses during a day.

  This is a slightly more general model than Taha's.

  Compare with the following models:
  * MiniZinc:  http://www.hakank.org/minizinc/bus_scheduling.mzn
  * Comet   :  http://www.hakank.org/comet/bus_schedule.co


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
:-lib(branch_and_bound).
:-lib(ic_search).


go :-
        % number of time slots
        Time_slots = 6,

        % demand: minimum number of buses at time t
        Demands = [](8, 10, 7, 12, 4, 4),

        collection_to_list(Demands, DemandsList),
        Max_num = sum(DemandsList),
        
        % result: how many buses start the schedule at time slot t?
        dim(X, [Time_slots]),
        X :: 0..Max_num,

        % the objective to minimize: the total number of buses
        collection_to_list(X, Vars),
        Num_buses #= sum(Vars),

        % meet the demands for this and the next time slot
        ( for(I, 1,Time_slots-1), param(X, Demands) do
              X[I]+X[I+1] #>= Demands[I]
        ),

        % demand "around the clock"
        X[Time_slots] + X[1] #= Demands[Time_slots],

        % search for minimum number of buses satisfying the constraints
        minimize(search(Vars, 0, first_fail, indomain, complete, []), Num_buses),

        % For all solutions
        % search(Vars, 0, occurrence, indomain_min, complete, []),
        % Num_buses #= 26,

        writeln([Num_buses, X]).
