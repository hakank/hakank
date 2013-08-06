/*

  Moving furnitures (scheduling) problem in SICStus Prolog.

   Marriott & Stuker: 'Programming with constraints', page  112f

  One result in ECLiPSe:
     Sp Sc Sb  St
    [0, 0, 30, 45]

  Where the values are the start time for each task:
   Starta med piano tid 0  : 3 persons  (30 min)
              chair tid 0  : 1 person   (10 min)
              bed   tid 30 : 3 persons  (15 min)
              table tid 45 : 2 persons  (15 min)

   0       10   15    30      45      60

   piano --------------|bed---|       
   piano --------------|       table----|
   piano --------------|bed---|
   chair --|            bed---|table----| 

  There are many other solutions...

  Compare with the following models:
  * ECLiPSE: http://www.hakank.org/eclipse/furniture_moving.ecl
  * MiniZinc: http://www.hakank.org/minizinc/furniture_moving.mzn
  * Comet: http://www.hakank.org/comet/furniture_moving.co
  * Choco: http://www.hakank.org/choco/FurnitureMoving.java
  * Gecode: http://www.hakank.org/gecode/furniture_moving.cpp
  * JaCoP: http://www.hakank.org/JaCoP/FurnitureMoving.java


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        move(_),
        fd_statistics.


go2 :-
        findall([Sp, Sc, Sb, St], move([Sp, Sc, Sb, St]),L),
        length(L, NumSolutions),
        write(start_times:L),nl,
        write(number_of_solutions:NumSolutions), nl,
        fd_statistics.


move([Sp, Sc, Sb, St]) :-

        StartTime = [Sp,Sc,Sb,St],
        domain(StartTime, 0,60),
        Duration = [30,10,15,15],
        MenNeeded = [3,1,3,2], % Resource needed

        length(Duration, Len),
        length(EndTime, Len),
        domain(EndTime, 0,60),
        
        % create Tasks
        %      Oi         Di        Ei       Hi                   Ti
        % task(StartTime, Duration, EndTime, ResourceConsumption, TaskID)
        (
            foreach(S, StartTime),
            foreach(D, Duration),
            foreach(E, EndTime),
            foreach(R, MenNeeded),
            foreach(task(S,D,E,R,0),Tasks)
        do
            true
        ),
        
        NumPersons = 3,
        cumulative(Tasks, [limit(NumPersons)]),
        
        % to get the end time of the tasks
        EndTime = [SpEnd,ScEnd,SbEnd,StEnd],
        %SpEnd #= Sp + 30,
        %ScEnd #= Sc + 10,
        %SbEnd #= Sb + 15,
        %StEnd #= St + 15,
       
        domain([End], 1, 60),
        maximum(End, EndTime),

        append([StartTime,EndTime],Vars),
        
        % get all solutions
        labeling([ff],Vars),
        % labeling([ff,minimize(End)],Vars),        

        write([start:StartTime,
               piano: Sp..SpEnd, chair: Sc..ScEnd, 
               bed: Sb..SbEnd, table: St..StEnd,
               end_time: EndTime, end:End]),
        nl.
        
        
