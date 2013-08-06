/*

  Set covering problem in SICStus Prolog.

  This example is from the OPL example covering.mod
  """
  Consider selecting workers to build a house. The construction of a 
  house can be divided into a number of tasks, each requiring a number of 
  skills (e.g., plumbing or masonry). A worker may or may not perform a 
  task, depending on skills. In addition, each worker can be hired for a 
  cost that also depends on his qualifications. The problem consists of 
  selecting a set of workers to perform all the tasks, while minimizing the 
  cost. This is known as a set-covering problem. The key idea in modeling 
  a set-covering problem as an integer program is to associate a 0/1 
  variable with each worker to represent whether the worker is hired. 
  To make sure that all the tasks are performed, it is sufficient to 
  choose at least one worker by task. This constraint can be expressed by a 
  simple linear inequality.
  """

  Solution from the OPL model:
  """
  Optimal solution found with objective: 14
  crew= {23 25 26}
  """

  Comet solution:
  objective tightened to: 14
  Crew={23,25,26}


  Compare with the following models:
  * Comet: http://www.hakank.org/comet/covering_opl.co
  * ECLiPSe: http://www.hakank.org/eclipse/covering_opl.ecl


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-

        NumWorkers = 32,
        Qualified = 
        [
            [ 1,  9, 19,  22,  25,  28,  31 ],
            [ 2, 12, 15, 19, 21, 23, 27, 29, 30, 31, 32 ],
            [ 3, 10, 19, 24, 26, 30, 32 ],
            [ 4, 21, 25, 28, 32 ],
            [ 5, 11, 16, 22, 23, 27, 31 ],
            [ 6, 20, 24, 26, 30, 32 ],
            [ 7, 12, 17, 25, 30, 31 ] ,
            [ 8, 17, 20, 22, 23  ],
            [ 9, 13, 14,  26, 29, 30, 31 ],
            [ 10, 21, 25, 31, 32 ],
            [ 14, 15, 18, 23, 24, 27, 30, 32 ],
            [ 18, 19, 22, 24, 26, 29, 31 ],
            [ 11, 20, 25, 28, 30, 32 ],
            [ 16, 19, 23, 31 ],
            [ 9, 18, 26, 28, 31, 32 ]
        ],

        length(Qualified,NumTasks),
        
        % cost per worker
        Cost = [1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 8, 9 ],


        % which workers to hire        
        length(Hire, NumWorkers),
        domain(Hire, 0,1),

        ( for(J,1,NumTasks), 
          param(Hire, Qualified) do
              nth1(J,Qualified,QualifiedJ),
              ( foreach(C,QualifiedJ),
                fromto(0,In,Out,Sum),param(Hire) do
                    element(C,Hire,HireC),
                    Out #= In + HireC
              ),
              Sum #>= 1
        ),
        
        scalar_product(Cost,Hire,#=,TotalCost),

        % search
        append(Hire,[TotalCost],Vars),
        labeling([ffc,enum,down,minimize(TotalCost)],Vars),


        write(total_cost:TotalCost),nl,
        get_solution(Hire, ToHire),
        write(to_hire:ToHire),nl,
        fd_statistics.


get_solution(List, Result) :-
        ( foreach(L,List),
          count(I,1,_N),
          fromto(Result,Out,In,[]) do
              L #= 1 -> Out = [I|In] ; Out = In
        ).