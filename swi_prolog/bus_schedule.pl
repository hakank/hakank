/*

  Bus scheduling in SWI Prolog

  Problem from Taha "Introduction to Operations Research", page 58.
  Scheduling of buses during a day.

  This is a slightly more general model than Taha's.

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
   %% number of time slots
   TimeSlots = 6,
   
   %% demand: minimum number of buses at time t
   Demands = [8, 10, 7, 12, 4, 4],      
   sum(Demands,#=,MaxNum),
   
   %% result: how many buses start the schedule at time slot t?
   length(X,TimeSlots),
   X ins 0..MaxNum,

   %% the objective to minimize: the total number of buses
   sum(X,#=,NumBuses),

   %% meet the demands for this and the next time slot.
   %% Around the corner as well (hence the rotation).
   rotate(X,XRotated),
   maplist(meet_demands,X,XRotated,Demands),
   
   %% search for minimum number of buses satisfying the constraints
   labeling([min(NumBuses)], X),

   writeln([NumBuses, X]).

%%
%% Require that this and the next timeslot meets the demands
%%
meet_demands(X1,X2,Demand) :-
        X1+X2 #>= Demand.

%%
%% rotate a list (put first element -> last)
%%
rotate(L,L2) :-
        L=[X|LRest], append(LRest,[X],L2).
        