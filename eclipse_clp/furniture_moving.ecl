/*
  Moving furnitures (scheduling) problem in ECLiPSe.

  Marriott & Stukey: 'Programming with constraints', page  112f

  One result in ECLiPSe:
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


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/
 

*/

:- lib(ic), lib(ic_search), lib(branch_and_bound), lib(ic_edge_finder).

go:-
        findall([Sp, Sc, Sb, St], move([Sp, Sc, Sb, St]),L),
        length(L, NumSolutions),
        write(start_times:L),nl,
        writeln(number_of_solutions:NumSolutions).


move([Sp, Sc, Sb, St]) :-

        NumPersons :: 0..3,

        Sp :: 0..30, % piano: 60 - 30 
        Sc :: 0..50, % chair: 60 - 10
        Sb :: 0..45, % bed  : 60 - 15
        St :: 0..45, % table: 60 - 15

        make_display_matrix([Sp, Sc, Sb, St], move), % only in tkeclipse

        Duration = [30,10,15,15],
        MenNeeded = [3,1,3,2],
        cumulative([Sp, Sc, Sb, St], Duration, MenNeeded, NumPersons),


        % to get the end time of the tasks
        SpEnd #= Sp + 30,
        ScEnd #= Sc + 10,
        SbEnd #= Sb + 15,
        StEnd #= St + 15,

        flatten([Sp,Sc,Sb,St,NumPersons], Vars),
        % minimize(search(Vars, 0, first_fail,indomain, complete, []),NumPersons),

        % get all solutions
        NumPersons = 3,
        search(Vars, 0, first_fail,indomain, complete, []),


        writeln([piano: Sp..SpEnd, chair: Sc..ScEnd, 
                 bed: Sb..SbEnd, table: St..StEnd, persons : NumPersons]).
        
        
