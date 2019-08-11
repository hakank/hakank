/*

  Assignment problems in SWI Prolog

  Different assignments problem, both minimization and maximization. 
  See the sources of the problem below.
 
  Compare to the following MiniZinc models, from which these problems
  are taken:
  * http://www.hakank.org/minizinc/assignment.mzn
  * http://www.hakank.org/minizinc/assignment2.mzn
  * http://www.hakank.org/minizinc/assignment2_2.mzn 
  * http://www.hakank.org/minizinc/assignment3.mzn
  * http://www.hakank.org/minizinc/assignment5.mzn
  * http://www.hakank.org/minizinc/assignment6.mzn


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

%%
%% Test all the assignment problems.
%%
go :-
        findall(P,cost(P,_,_),Ps),
        max_list(Ps,NumProblems),
        between(1,NumProblems,N),
        once(assignment(N,Z,A)),
        writeln(z=Z),
        writeln(assignment=A),
        nl,
        fail,
        nl.

go.


%
% assignment(ProblemNumber)
% 
assignment(Problem, TotalCost, Assignments) :-

    cost(Problem, Mode, Cost),
    (
     var(TotalCost)
    ->
     format("\nProblem ~d\n", Problem)
    ),
    
    % get the dimension of the problem
    length(Cost, Rows),
    transpose(Cost,CostT),
    length(CostT,Cols),

    % decision variables: a 0..1 matrix
    new_matrix(Rows,Cols,0..1, X),
    flatten(X,Vars),
    
    % exacly one assignment per row, all rows must be assigned
    row_col_assignment(X,#=,1),
    
    % zero or one assignments per column
    transpose(X, XT),
    row_col_assignment(XT,#=<,1),

    
    % calculate TotalCost
    % TotalCost #= sum([X[I,J]*Cost[I,J] : I in 1..Rows, J in 1..Cols]),
    flatten(Cost, CostFlatten),
    scalar_product(CostFlatten,Vars,#=,TotalCost),
    
    % prepare for maximization (if needed)
    TotalCostNeg #= -TotalCost,
    
    %
    % get the optimization mode
    %
    (
     Mode == minimize
    ->
     OptValue #= TotalCost 
    ;
     OptValue #= TotalCostNeg
    ),

    %%
    %% search
    %%
    (
     var(TotalCost)
    ->
     writeln(Mode), 
     once(labeling([min(OptValue)], Vars))
    ;
     labeling([], Vars)
    ),
    % print_matrix(X),
    % writeln(totalCost=TotalCost),
    
    %
    % get the assignments.
    %
    findall(J,
            (between(1,Rows,I),
             between(1,Cols,J),
             matrix_element(X,I,J,1)
            ),
            Assignments),
    nl.


%%
%% exacly one assignment per row, all rows must be assigned
%% foreach(I in 1..Rows) sum([X[I,J] : J in 1..Cols]) #= 1 end,
%%
row_col_assignment([],_Rel,_Value).
row_col_assignment([X|Xs],Rel,Value) :-
        sum(X,Rel,Value),
        row_col_assignment(Xs,Rel,Value).


%
% cost(ProblemNumber, OptimizationMode, CostMatrix). 
%

% Data from 
% Winston "Operations Research", Assignment Problems, page 393f
% added the fifth column
% See http://www.hakank.org/minizinc/assignment.mzn
cost(1, Op, M) :-
        Op = minimize, 
        M = [[14,  5, 8,  7, 15],
             [ 2, 12, 6,  5,  3],
             [ 7,  8, 3,  9,  7],
             [ 2,  4, 6, 10,  1]].


% 
% Winston "Operations Research", page 398, swimming team example
% (original version]
% See http://www.hakank.org/minizinc/assignment2.mzn 
% 
cost(2, Op, M) :-
        Op = minimize, 
        M = [[54, 54, 51, 53], 
             [51, 57, 52, 52],
             [50, 53, 54, 56],
             [56, 54, 55, 53]].


% 
% Winston "Operations Research", page 398, swimming team example
% See http://www.hakank.org/minizinc/assignment2_2.mzn 
% expanded version
%
cost(3, Op, M) :-
        Op = minimize, 
        M = [[54, 54, 51, 53,   50,60,70,80,90,100], 
             [51, 57, 52, 52,   40,50,60,70,80, 90],
             [50, 53, 54, 56,   40,50,60,80,93, 69],
             [56, 54, 55, 53,   60,80,40,60,50,100]].


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
cost(4, Op, M) :-
        Op = maximize, M = 
        [[7, 5, 8, 2],
         [7, 8, 9, 4],
         [3, 5, 7, 9],
         [5, 5, 6, 7]].


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
cost(5, Op, M) :-
        Op = minimize, 
        M = [[4, 1, 3, 5, 2],
             [2, 1, 3, 4, 5],
             [3, 2, 4, 1, 5],
             [2, 3, 4, 5, 1],
             [4, 2, 3, 1, 5]].

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
cost(6,Op, M) :-
        Op = minimize, 
        M = [[13, 21, 20, 12,  8, 26, 22, 11],
             [12, 36, 25, 41, 40, 11,  4,  8],
             [35, 32, 13, 36, 26, 21, 13, 37],
             [34, 54,  7,  8, 12, 22, 11, 40],
             [21,  6, 45, 18, 24, 34, 12, 48],
             [42, 19, 39, 15, 14, 16, 28, 46],
             [16, 34, 38,  3, 34, 40, 22, 24],
             [26, 20,  5, 17, 45, 31, 37, 43]].

