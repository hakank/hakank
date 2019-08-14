/*

  Scheduling in SWI Prolog

  Example from SICStus Prolog:
  http://www.sics.se/sicstus/docs/latest/html/sicstus/Cumulative-Scheduling.html#Cumulative%20Scheduling

  """
  Cumulative Scheduling

  This example is a very small scheduling problem. We consider seven
  tasks where each task has a fixed duration and a fixed amount of used
  resource:

  Task Duration Resource
   t1    16       2
   t2     6       9
   t3    13       3
   t4     7       7
   t5     5      10
   t6    18       1
   t7     4      11

  The goal is to find a schedule that minimizes the completion time for
  the schedule while not exceeding the capacity 13 of the resource. The
  resource constraint is succinctly captured by a cumulative/4
  constraint. Branch-and-bound search is used to find the minimal
  completion time. 

  This example was adapted from [Beldiceanu & Contejean 94]. 
  """

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).


go :-
        problem(1,Duration,Resource,Capacity,StartTimeMax),
        schedule1(Duration,Resource, Capacity, StartTimeMax, StartTimes, MaxEndTime),
        writeln([StartTimes, MaxEndTime]).

schedule1(Duration,Resource, Capacity, StartTimeMax, StartTimes, MaxEndTime) :-
        
        length(Duration,Len),
        length(StartTimes,Len),     
        StartTimes ins 1..StartTimeMax,
       
        maplist(create_task,StartTimes,Duration,Resource,Tasks,EndTimes),
        cumulative(Tasks,[limit(Capacity)]),
        max_list_clp(EndTimes,MaxEndTime),
        
        labeling([min(MaxEndTime)],StartTimes).

problem(1,Duration,Resource,Capacity,StartTimeMax) :-
        Duration = [16, 6,13, 7, 5,18, 4],
        Resource = [ 2, 9, 3, 7,10, 1,11],
        Capacity = 13,
        StartTimeMax = 30.
        
