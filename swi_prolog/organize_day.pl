/*

  Organize a day in SWI Prolog

  From Andy King: "(Finite domain) constraint logic programming"
  https://eclipseclp.org/reports/eclipse.ppt
  (Slide 38: "How to organise your day")
  
  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-

   TasksStr = ["Work","Mail","Shop","Bank"],
   length(TasksStr,N),
   
   length(Begins,N),
   Begins ins 9..17,
   length(Ends,N),
   Ends ins 9..17,

   durations(Durations),
   before_tasks(BeforeTasks), 

   findall([Begins,Ends], organize(Durations,BeforeTasks,Begins, Ends),L),
   forall(member([BB,EE],L),
          (
           forall(between(1,N,Task),
                   (
                    nth1(Task,TasksStr,S),
                    nth1(Task,BB,B),
                    nth1(Task,EE,E),
                    format("~w: ~d .. ~d~n",[S,B,E])
                    )
                 ),
           nl
          )
         ),
   nl.


organize(Durations,BeforeTasks,Begins,Ends) :-

   maplist(begin_plus_duration,Begins,Durations,Ends),
   
   %% no_overlaps
   serialized(Begins,Durations),

   % handle precendeces
   maplist(precedence(Begins,Ends),BeforeTasks),
   
   % Work >= 11 a clock
   element(1,Begins,Begins1),
   Begins1 #>= 11,
   
   flatten([Begins,Ends],Vars),

   label(Vars).


begin_plus_duration(Begin,Duration,End) :-
        End #= Begin + Duration.

precedence(Begins,Ends,[A,B]) :-
        element(A,Ends,EndsA),
        element(B,Begins,BeginsB),
        EndsA #=< BeginsB.

   
% duration of the four tasks
durations(Durations) :-
        Durations = [4,1,2,1].

% precedences
% [A,B] : task A must be completed before task B
before_tasks(Before) :-
        Before = [[4,3],[2,1]].
