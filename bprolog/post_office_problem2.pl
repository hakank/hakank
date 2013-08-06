/*

  Post office problem in B-Prolog.

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

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/




go :-
        Days = 7,
        % requirements number workers per day
        Need = [17, 13, 15, 19, 14, 16, 11],

        % Total cost for the 5 day schedule.
        % Base cost per day is 100.
        % Working saturday is 100 extra
        % Working sunday is 200 extra.
        Cost = [500, 600, 800, 800, 800, 800, 700],

        % Decision variables. X[I]: Number of workers starting at day I
        length(X,Days),
        X :: 0..10,

        % TotalCost: sum of total workers (started on a day)
        % Week start at monday (day 0)
        % Note: We use 0..6 as days since it's easier with the 
        % modulo operations.
        foreach((Nd,I) in (Need,0..6),
                 [I1,I5,I6,XI1,ThisSum],
                 (
                     I1 is I+1,
                     I5 is ((I + 5) mod 7),
                     I6 is ((I + 6) mod 7),
                     element(I1,X,XI1),
                     ThisSum #= sum([XJ : J in 0..6,[XJ,J1],
                                 (J \= I5,
                                  J \= I6,
                                  J1 is J+1,
                                  element(J1,X,XJ) )]),
                    ThisSum #>= Nd % Satisfy the need of the day
                )
        ),

        scalar_product(Cost,X,#=,TotalCost),
        NumWorkers #= sum(X),

        labeling([constr,split,minimize(TotalCost)],X),

        write(x:X),nl,
        write(total_cost:TotalCost),nl,
        write(num_workers:NumWorkers),nl,nl.

