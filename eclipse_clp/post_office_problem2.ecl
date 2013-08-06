/*

  Post office problem in ECLiPSe.

  Problem statement:
  http://www-128.ibm.com/developerworks/linux/library/l-glpk2/

  From Winston "Operations Research: Applications and Algorithms":
  """
  A post office requires a different number of full-time employees working
  on different days of the week [summarized below]. Union rules state that
  each full-time employee must work for 5 consecutive days and then receive
  two days off. For example, an employee who works on Monday to Friday
  must be off on Saturday and Sunday. The post office wants to meet its
  daily requirements using only full-time employees. Minimize the number
  of employees that must be hired.

  To summarize the important information about the problem:

    * Every full-time worker works for 5 consecutive days and takes 2 days off
    * Day 1 (Monday): 17 workers needed
    * Day 2 : 13 workers needed
    * Day 3 : 15 workers needed
    * Day 4 : 19 workers needed
    * Day 5 : 14 workers needed
    * Day 6 : 16 workers needed
    * Day 7 (Sunday) : 11 workers needed

  The post office needs to minimize the number of employees it needs
  to hire to meet its demand.
  """

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/post_office_problem2.mzn
  * SICStus: http://www.hakank.org/sicstus/post_office_problem2.pl

  Mats Carlsson (SICStus) suggest the alternative solution in go2/0.


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(listut).
:-lib(branch_and_bound).
%:-lib(ic_global).
%:-lib(ic_search).

%:-lib(propia).

go :-
        Days = 7,
        % requirements number workers per day
        Need = [17, 13, 15, 19, 14, 16, 11],

        % Total cost for the 5 day schedule.
        % Base cost per day is 100.
        % Working saturday is 100 extra
        % Working sunday is 200 extra.
        Cost = [500, 600, 800, 800, 800, 800, 700],

        Selection = max_regret,
        Choice = indomain_middle,
        post_office(Selection,Choice,Days,Need,Cost,X,NumWorkers,
                          TotalCost,Backtracks),
        writeln([X,NumWorkers,TotalCost,Backtracks]).


post_office(Selection,Choice,Days,Need,Cost,X,NumWorkers,TotalCost,Backtracks) :-
        writeln([selection:Selection, choice:Choice]),

        % Decision variables. X[I]: Number of workers starting at day I
        length(X,Days),
        X :: 0..10,

        % Week start at monday (day 0)
        % Note: We use 0..6 as days since it's easier with the 
        % modulo operations.
        ( foreach(Nd,Need),
          count(I,0,_),
          param(X) do
              I5 is ((I + 5) mod 7),
              I6 is ((I + 6) mod 7),
              ( count(J,0,6),
                fromto(0,In,Out,ThisSum),
                param(X,I5,I6) do
                    J \= I5,
                    J \= I6
              ->
                (
                    J1 is J+1,
                    nth1(J1,X,XJ),
                    Out #= In + XJ
                )
              ;
                Out = In
              ),
              ThisSum #>= Nd % Satisfy the need of the day
        ),

        % TotalCost: sum of total workers (started on a day)
        TotalCost #= Cost*X,
        sum(X) #= NumWorkers,

        minimize(search(X,0,Selection,Choice,complete,[backtrack(Backtracks)]), TotalCost),

        write(x:X),nl,
        write(total_cost:TotalCost),nl,
        write(num_workers:NumWorkers),nl,
        write(backtracks:Backtracks),nl.

