/*

  Scheduling example in ECLiPSe.

  Example fromm
   http://www.sciences.univ-nantes.fr/info/perso/permanents/monfroy/Teaching/Cours/ConstraintLecture-PS-PDF/P1-CP_Lecture/Part13_global_reified_constraints/global_reified_constraints.pdf
  page 32,

  Found a solution with cost 28
  Found a solution with cost 27
  Found a solution with cost 23
  [1, 17, 10, 10, 5, 5, 1]
  23


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/


*/

:-lib(ic),lib(ic_global),lib(branch_and_bound), lib(ic_cumulative).

go:-
        schedule(LO, End),
        write(LO), nl,
        write(End), nl.

schedule(LO,End):-
        % starting time
        LO = [O1,O2,O3,O4,O5,O6,O7],
 
        %duration of tasks
        LD = [16,6,13,7,5,18,4],
        
        % resources needed by each task
        LR = [2,9,3,7,10,1,11],
        % ending times
        LE = [E1,E2,E3,E4,E5,E6,E7],
        
        % time allowed
        End:: [1..30],
        LO:: [1..30],
        LE:: [1..30],

        % ending time is starting time + duration
        O1 + 16 #= E1,
        O2 + 6 #= E2,
        O3 + 13 #= E3,
        O4 + 7 #= E4,
        O5 + 5 #= E5,
        O6 + 18 #= E6,
        O7 + 4 #= E7,
        
        % constraint End to be the maximum element in the li
        ic:maxlist(LE,End),
        
        % start, duration, resource units, resource limits
        cumulative(LO,LD,LR,13),
        
        % find the values that minize LO
        minimize(labeling(LO),End).


