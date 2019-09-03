/*

  Scheduling example in SWI Prolog

  Example from
  http://www.sciences.univ-nantes.fr/info/perso/permanents/monfroy/Teaching/Cours/ConstraintLecture-PS-PDF/P1-CP_Lecture/Part13_global_reified_constraints/global_reified_constraints.pdf
  page 32,

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).


go:-
        schedule(Starts, End),
        writeln(Starts),
        writeln(End),
        nl.

schedule(Starts,End):-

        %% duration of tasks
        Durations = [16,6,13,7,5,18,4],
        
        %% resources needed by each task
        Resources = [2,9,3,7,10,1,11],

        Limit = 13,
        
        length(Durations, Len),
        
        %% starting time
        length(Starts, Len),
        Starts ins 1..30,

        %% ending times
        length(Ends, Len),
        Ends ins 1..30,

        % time allowed
        End in 1..30,
        
        %% ending time is starting time + duration
        maplist(make_end,Starts,Durations,Ends),
        
        % constraint End to be the maximum element in the li
        max_list_clp(Ends, End),
        
        % start, duration, resource units, resource limits
        my_cumulative(Starts,Durations,Resources,Limit),
        
        %% find the values that minize Ends
        labeling([min(End)],Starts).

make_end(Start,Duration,End) :-
        End #= Start + Duration.

