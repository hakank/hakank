/*

  Scheduling in ECLiPSe.

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

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/


:- lib(ic).
:- lib(ic_cumulative).
:- lib(branch_and_bound).
:- lib(lists).

schedule(Ss, End,Capacity) :-
        length(Ss, 7),
        Ds = [16, 6,13, 7, 5,18, 4],
        Ss :: 1..30,

        Rs = [ 2, 9, 3, 7,10, 1,11],
        End :: 1..50,

        after(Ss, Ds, End),
        cumulative(Ss, Ds, Rs, Capacity),
        append(Ss, [End], Vars),

        minimize(labeling(Vars),End).

after([], [], _).
after([S|Ss], [D|Ds], E) :- E #>= S+D, after(Ss, Ds, E).


go :- 
        Capacity is 13,
        schedule(Ss, End, Capacity),
        write(capacity: Capacity),nl,
        write([Ss, End]), nl.
