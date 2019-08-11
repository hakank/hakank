/*

  Furniture moving (scheduling) in SWI Prolog

  From Marriott & Stukey: "Programming with constraints", page  112f

  Different furniture takes different times and number of people:
   - piano:  3 persons 30 min
   - chair:  1 person  10 min
   - bed  :  3 persons 15 min
   - table:  2 persons 15 min
  
  Here is one solution using 4 people:
     Sp Sc Sb  St
    [0, 0, 30, 45]

  Where the values are the start time for each task:
   Starts with piano time 0  : 3 persons  (30 min)
               chair time 0  : 1 person   (10 min)
               bed   time 30 : 3 persons  (15 min)
               table time 45 : 2 persons  (15 min)

   0       10   15    30      45      60

   piano --------------|bed---|       
   piano --------------|       table----|
   piano --------------|bed---|
   chair --|            bed---|table----| 

  There are many other solutions...

  Note: The examples below use 3 persons.
  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).


go :-
        move([Sp, Sc, Sb, St]),
        writeln([Sp, Sc, Sb, St]),
        nl.

go2 :-
        findall([Sp, Sc, Sb, St], move([Sp, Sc, Sb, St]), L),
        length(L, NumSolutions),
        writeln(numSolutions=NumSolutions),
        nl.

%
% Minimize end time (makespan). 
% This is a slightly different problem.
% 
go3 :-
        % How does the number of people influence the
        % total time?
        NumPeople in 3..10,
        indomain(NumPeople),
        
        writeln(numPeople=NumPeople),
        move2(NumPeople,MaxEnd),
        writeln(maxEnd=MaxEnd),
        nl,

        fail,
        nl.

go3.

%
% Scheduling furniture moving.
% 
% 
move([Sp, Sc, Sb, St]) :-

        Sp in 0..30, % piano
        Sc in 0..50, % chair
        Sb in 0..45, % bed
        St in 0..45, % table

        Starts = [Sp,Sc,Sb,St],
        
        % to get the end time of the tasks
        Ends = [Ep,Ec,Eb,Et],
        Ends ins 0..100,
        
        % Duration = [30,10,15,15],
        % MenNeeded = [3,1,3,2],
        Tasks = [task(Sp, 30, Ep, 3, 1), % piano
                 task(Sc, 10, Ec, 1, 2), % chair
                 task(Sb, 15, Eb, 3, 3), % bed
                 task(St, 15, Et, 2, 4)  % table
                ],
        
        % Note: Limit must be an integer and cannot be decision variable.
        % limit(3): We have in total 3 persons.
        cumulative(Tasks, [limit(3)]),
        
        append([Starts,Ends], Vars),
        labeling([], Vars),

        writeln([piano:(Sp,Ep), chair:(Sc,Ec),  bed:(Sb,Eb), table:(St,Et)]).

%
% Minimize end time (makespan). 
% This is a slightly different problem: larger domains.
% 
move2(NumPeople, MaxEnd) :-

        % Larger domains
        Starts = [Sp,Sc,Sb,St], % [piano,chair,bed,table]
        Starts ins 0..100,
        
        % to get the end time of the tasks
        Ends = [Ep,Ec,Eb,Et],
        Ends ins 0..100,

        % Duration = [30,10,15,15],
        % MenNeeded = [3,1,3,2],
        % Note: Here we require 2 persons to move the chair (not 1 as in the first example)
        Tasks = [task(Sp, 30, Ep,3, 1), % piano
                 task(Sc, 10, Ec,2, 2), % chair (here we require 2 persons)
                 task(Sb, 15, Eb,3, 3), % bed
                 task(St, 15, Et,2, 4)  % table
                ],

        MaxEnd in 0..100,
        max_list_clp(Ends, MaxEnd),
        
        % Note: Limit must be an integer and cannot be decision variable.
        cumulative(Tasks, [limit(NumPeople)]),
        
        append([Starts,Ends], Vars),
        once(labeling([ff,bisect,min(MaxEnd)], Vars)),

        writeln(['piano':(Sp,Ep), 'chair':(Sc,Ec),  'bed':(Sb,Eb), 'table':(St,Et),'maxEnd':MaxEnd]).

