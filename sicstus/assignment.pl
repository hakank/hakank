/*

  Assignment problems in SICStus Prolog.

  Different assignments problem, both minimization and maximization. 
  See the sources of the problem below.
 
  Compare with the following models:
  - MiniZinc:
   * http://www.hakank.org/minizinc/assignment.mzn
   * http://www.hakank.org/minizinc/assignment2.mzn
   * http://www.hakank.org/minizinc/assignment2_2.mzn 
   * http://www.hakank.org/minizinc/assignment3.mzn
   * http://www.hakank.org/minizinc/assignment5.mzn
   * http://www.hakank.org/minizinc/assignment6.mzn

  - Comet:
   * http://www.hakank.org/comet/assignment.co
   * http://www.hakank.org/comet/assignment2.co
   * http://www.hakank.org/comet/assignment3.co
   * http://www.hakank.org/comet/assignment6.co


  This SICStus Prolog model was, almost verbatim with the
  appropriate changes, based on the ECLiPSe model:
   * http://www.hakank.org/eclipse/assignment.ecl


  The alternative solution in assignment2 was suggested
  by Mats Carlsson.

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :-
        assignment(1),
        assignment(2),
        assignment(3),
        assignment(4),
        assignment(5),
        assignment(6).

% Compare my version (assignment) and Mats Carlsson's (assignment2/0).
go2 :-
        ( for(A,1,6) do
              assignment(A),
              assignment2(A) % Mats Carlsson'2 version
        ).

% pretty prints a matrix
pretty_print(X) :-
        (
            foreach(Rows,X) 
        do
            write(Rows), nl
        ).


% Suggested by Mats Carlsson
matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).


%
% assignment(ProblemNumber)
% 
assignment(Problem) :-

        cost(Problem, Mode, Cost),
        format("\nProblem ~d\n", Problem),
        
        % get the dimension of the problem
        matrix(Cost, [Rows, Cols]),

        % decision variables: a 0..1 matrix
        matrix(X, [Rows, Cols]),
        append(X, Vars),
        domain(Vars, 0, 1),

        % exacly one assignment per row, all rows must be assigned
        ( foreach(Row, X) do
              sum(Row,#=, 1)
        ),

        % zero or one assignments per column
        transpose(X, Columns),
        ( foreach(Column, Columns) do
              sum(Column, #=<, 1)
        ),

        % calculate TotalCost
        append(Cost, CostFlattened),
        scalar_product(CostFlattened, Vars, #=, TotalCost),

        % prepare for maximization (if needed)
        % TotalCostNeg #= -TotalCost
        TotalCostNeg #= TotalCost * (-1),
       
        % get the optimization mode
        (
            Mode = minimize
        -> 
            write('minimize'), nl, OptValue #= TotalCost 
        ; 
            write('maximize'), nl, OptValue #= TotalCostNeg
        ),

        % search
        labeling([ff,bisect,down,minimize(OptValue)],Vars),

        % get the assignments 
        %
        % note: this should be done _after_ labeling.
        length(Assignments, Rows),
        (
            foreach(Row, X),
            foreach(A, Assignment), 
            fromto(Assignments, Out, In, []),
            param(X,Cols) do
                 element(K, Row, 1),
                 Out = [K|In]
        ),

        % Print the results.
        pretty_print(X),        
        write(assignments:Assignments),nl,
        write([total_cost:TotalCost]), nl,nl,
        fd_statistics, nl.



% This version using global_cardinaliry was suggested by 
% Mats Carlsson
assignment2(Problem) :-
        cost(Problem, Mode, CostRows),
        format('\nProblem ~d\n~w\n', [Problem,Mode]),
        transpose(CostRows, CostCols),
        length(CostRows, R),
        length(CostCols, C),
        length(Vars, R),
        domain(Vars, 1, C),
        (   for(I,1,C),
            foreach(I-Y,Ys)
        do  Y in 0..1
        ),
        global_cardinality(Vars, Ys, [cost(Cost,CostRows)]),
        (Mode==minimize -> Dir=up ; Dir=down),
        labeling([Dir], [Cost]),
        labeling([], Vars), !,
        format('assignment = ~w, cost = ~d\n', [Vars,Cost]),
        fd_statistics, nl.


%
% The problems below are defined as
% 
%   cost(ProblemNumber, OptimizationMode, CostMatrix)
% where OptimizationMode is either minimize or maximize
%

% Data from 
% Winston "Operations Research", Assignment Problems, page 393f
% added the fifth column
% See http://www.hakank.org/minizinc/assignment.mzn
cost(1, minimize, [[14,  5, 8,  7, 15],
                   [2, 12, 6,  5,  3],
                   [7,  8, 3,  9,  7],
                   [2,  4, 6, 10,  1]]).


% 
% Winston "Operations Research", page 398, swimming team example
% (original version)
% See http://www.hakank.org/minizinc/assignment2.mzn 
% 
cost(2, minimize, [[54, 54, 51, 53], 
                   [51, 57, 52, 52],
                   [50, 53, 54, 56],
                   [56, 54, 55, 53]]).


% 
% Winston "Operations Research", page 398, swimming team example
% See http://www.hakank.org/minizinc/assignment2_2.mzn 
% expanded version
%
cost(3, minimize, [[54, 54, 51, 53,   50,60,70,80,90,100], 
                   [51, 57, 52, 52,   40,50,60,70,80, 90],
                   [50, 53, 54, 56,   40,50,60,80,93, 69],
                   [56, 54, 55, 53,   60,80,40,60,50,100]]).


%
% Winston "Operations Research", page 399
% 
% """
% Tom Cruise, Freddy Prinze Jr, Harrison Ford, and Matt LeBlanc
% are marooned on a desert island with Jennifer Anniston,
% Courtney Cos, Gwynneth Paltrow, and Julia Roberts.
% The 'compatibility matrix' in Table 52 indicate how much happiness
% each couple would experience if the spend all their time toghether.
% The happiness earned by a couple is proportional to the fraction 
% of time the spend toghether. 
% ...
% The optimal solution requires that that each person send all their
% time with one person of the opposite sex, so this result is often
% referred to as the Marriage Theorem.
% """
%
% See http://www.hakank.org/minizinc/assignment3.mzn
%
% males:
% 1 "Tom Cruise"
% 2 "Freddie Prinz Jr"
% 3 "Harrison Ford"
% 4 "Mark LeBlanc"
%
% females:
% 1 "Jennifer Anniston"
% 2 "Courtney Cox"
% 3 "Gwynneth Paltrow"
% 4 "Julia Roberts"
%
cost(4, maximize, [[7, 5, 8, 2],
                   [7, 8, 8, 4],
                   [3, 5, 7, 9],
                   [5, 5, 6, 7]]).


% From
%  "SAS OR 9.1 User's Guide Mathematical Programming"
% """
% Consider assigning five programmers to five programming jobs. Each
% programmer prefers specific programming job over others. [...] 
% Suppose you ask each programmer to rank the jobs according to preference
% (using 1 for the most preferred job and 5 for the least preffered job].
% PROC ASSIGN maximizes the total preference of the group by minimizing the
% sum of the preferences. 
% 
%    PROGRAMMER     JOB1 JOB2 JOB3 JOB4 JOB5
%    PROGRAMMER1    4    1    3    5    2
%              2    2    1    3    4    5
%              3    3    2    4    1    5
%              4    2    3    4    5    1
%              5    4    2    3    1    5
% 
% """
% 
% See http://www.hakank.org/minizinc/assignment5.mzn
% 
cost(5, minimize, [[4, 1, 3, 5, 2],
                   [2, 1, 3, 4, 5],
                   [3, 2, 4, 1, 5],
                   [2, 3, 4, 5, 1],
                   [4, 2, 3, 1, 5]]).



%
% From GLPK:s example assign.mod:
% """
% The assignment problem is one of the fundamental combinatorial
% optimization problems.
%
% In its most general form, the problem is as follows:
%
% There are a number of agents and a number of tasks. Any agent can be
% assigned to perform any task, incurring some cost that may vary
% depending on the agent-task assignment. It is required to perform all
% tasks by assigning exactly one agent to each task in such a way that
% the total cost of the assignment is minimized.
%
% (From Wikipedia, the free encyclopedia.] 
% """
% 
% """
% These data correspond to an example from [Christofides].
% """
%
% See http://www.hakank.org/minizinc/assignment6.mzn
%
cost(6,minimize, [[13, 21, 20, 12,  8, 26, 22, 11],
                  [12, 36, 25, 41, 40, 11,  4,  8],
                  [35, 32, 13, 36, 26, 21, 13, 37],
                  [34, 54,  7,  8, 12, 22, 11, 40],
                  [21,  6, 45, 18, 24, 34, 12, 48],
                  [42, 19, 39, 15, 14, 16, 28, 46],
                  [16, 34, 38,  3, 34, 40, 22, 24],
                  [26, 20,  5, 17, 45, 31, 37, 43]]).
