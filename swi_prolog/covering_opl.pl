/*

  Set covering problem (OPL) in SWI Prolog

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


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-
        NumWorkers = 32,

        %% The workers qualified for the works
        Qualified = 
        [
            [  1,  9, 19, 22, 25, 28, 31 ],
            [  2, 12, 15, 19, 21, 23, 27, 29, 30, 31, 32 ],
            [  3, 10, 19, 24, 26, 30, 32 ],
            [  4, 21, 25, 28, 32 ],
            [  5, 11, 16, 22, 23, 27, 31 ],
            [  6, 20, 24, 26, 30, 32 ],
            [  7, 12, 17, 25, 30, 31 ] ,
            [  8, 17, 20, 22, 23  ],
            [  9, 13, 14, 26, 29, 30, 31 ],
            [ 10, 21, 25, 31, 32 ],
            [ 14, 15, 18, 23, 24, 27, 30, 32 ],
            [ 18, 19, 22, 24, 26, 29, 31 ],
            [ 11, 20, 25, 28, 30, 32 ],
            [ 16, 19, 23, 31 ],
            [  9, 18, 26, 28, 31, 32 ]
        ],
        
        %% cost per worker
        Cost = [1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, 
                3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 7, 8, 9 ],


        %% which workers to hire        
        length(Hire,NumWorkers),
        Hire ins 0..1,

        maplist(check_hired(Hire),Qualified),       
        scalar_product(Cost,Hire,#=,TotalCost),

        % search
        flatten(Hire,Vars),
        labeling([min(TotalCost),ffc],Vars),

        writeln(total_cost=TotalCost),
        writeln(hire=Hire),
        findall(H,(between(1,NumWorkers,H), nth1(H,Hire,1)), ToHire),
        writeln(to_hire=ToHire),
        nl.

%%
%% Ensure that for each job there is at least one qualified
%%
check_hired(Hire,Jobs) :-
        % findall(HJ,(member(H,Job),writeln(h=H),element(H,Hire,HJ)),Qualified), % don't work
        extract_from_indices(Jobs,Hire,Qualified),
        sum(Qualified,#>=,1).
