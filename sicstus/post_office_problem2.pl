/*

  Post office problem in SICStus Prolog.

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

  Compare with the following model:
  * MiniZinc: http://www.hakank.org/minizinc/post_office_problem2.mzn

  Mats Carlsson suggest the alternative solution in go2/0.
  

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).


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
        domain(X,0,10),

        % TotalCost: sum of total workers (started on a day)
        % Week start at monday (day 0)
        % Note: We use 0..6 as days since it's easier with the 
        % modulo operations.
        ( foreach(Nd,Need),
          count(I,0,_),
          param(X) do
              I1 is I+1,
              I5 is ((I + 5) mod 7),
              I6 is ((I + 6) mod 7),
              element(I1,X,XI1),
              ( count(J,0,6),
                fromto(0,In,Out,ThisSum),
                param(X,I,XI1,I5,I6) do
                    J \= I5,
                    J \= I6
              ->
                (
                    J1 is J+1,
                    element(J1,X,XJ),
                    Out #= In + XJ
                )
              ;
                Out = In
              ),
              ThisSum #>= Nd % Satisfy the need of the day
        ),
        scalar_product(Cost,X,#=,TotalCost),
        sum(X,#=,NumWorkers),

        labeling([min,bisect,down,minimize(TotalCost)],X),

        write(x:X),nl,
        write(total_cost:TotalCost),nl,
        write(num_workers:NumWorkers),nl,nl,
        fd_statistics.


% This alternative and much neater solution was suggested by
% Mats Carlsson.
go2 :-
        Days = 7,
        % requirements number workers per day
        Need = [17, 13, 15, 19, 14, 16, 11],
        % Total cost for the 5 day schedule.
        % Base cost per day is 100.
        % Working saturday is 100 extra
        % Working sunday is 200 extra.
        Cost = [500, 600, 800, 800, 800, 800, 700],
        % Decision variables. X[I]: Number of workers starting at day I
        length(X, Days),
        domain(X, 0, 10),
        append(X, X, [_,_,_|Ys]),
        (   foreach(Nd,Need),
            fromto(Ys, Ys1, Ys2, _)
        do  Ys1 = [Y1|Ys2],
            Ys2 = [Y2,Y3,Y4,Y5|_],
            Y1+Y2+Y3+Y4+Y5 #>= Nd
        ),
        scalar_product(Cost, X, #=, Total),
        labeling([minimize(Total),bisect], X),
        format('assignment = ~w, total cost = ~d\n', [X,Total]),
        fd_statistics.
