/*

   Assignment problems in ECLiPSe.

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


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(matrix_util).
% :-lib(ic_global).
:-lib(ic_global_gac). % for gcc
:-lib(ic_search).
:-lib(branch_and_bound).
%:-lib(propia).


% pretty prints a matrix
pretty_print(X) :-
        dim(X, [Rows,Cols]),
        ( for(I, 1, Rows), param(X, Cols) do
            ( for(J, 1, Cols), param(X, I) do
                XX is X[I,J],
                printf("%2d", XX),
                write(" ")
            ),
            nl
        ).        

go :-        
        N = 6,
        ( for(I,1,N) do
              assignment(I)
        ).


%
% assignment(ProblemNumber)
% 
assignment(Problem) :-

        cost(Problem, Mode, Cost),
        printf("\nProblem %d\n", [Problem]),
        
        % get the dimension of the problem
        dim(Cost,[Rows,Cols]),

        % decision variables: a 0..1 matrix
        dim(X,[Rows,Cols]),
        X :: 0..1,

        % exacly one assignment per row, all rows must be assigned
        ( for(I,1,Rows), param(X,Cols) do sum(X[I,1..Cols]) #= 1
        ),

        % zero or one assignments per column
        ( for(J,1,Cols), param(X,Rows) do sum(X[1..Rows,J]) #=< 1
        ),

        % calculate TotalCost
        ( for(I,1,Rows) * for(J,1,Cols), 
          fromto(0,In,Out,TotalCost),
          param(X,Cost) do Out #= In + X[I,J]*Cost[I,J]
        ),

        % prepare for maximization (if needed)
        TotalCostNeg #= -TotalCost,

        %
        % get the optimization mode
        %
        (
            Mode = minimize
        -> 
            writeln("minimize"), OptValue #= TotalCost 
        ; 
            writeln("maximize"), OptValue #= TotalCostNeg
        ),


        %
        % search
        % 
        term_variables(X,Vars),        
        minimize(search(Vars,0,first_fail,indomain,complete,[]), OptValue),


        %
        % get the assignments 
        %
        % note: this should be done _after_ labeling.
        %
        length(Assignments, Rows),
        (for(I,1,Rows), 
         fromto(Assignments, Out, In, []),
         param(X,Cols) do
             ThisRow is X[I,1..Cols],
             element(K, ThisRow, 1),
             Out = [K|In]
        ),

        pretty_print(X),
        writeln(assignments:Assignments),
        writeln(total_cost:TotalCost).


%
% cost(ProblemNumber, OptimizationMode, CostMatrix). 
%

% Data from 
% Winston "Operations Research", Assignment Problems, page 393f
% added the fifth column
% See http://www.hakank.org/minizinc/assignment.mzn
cost(1, minimize, []([](14,  5, 8,  7, 15),
                     []( 2, 12, 6,  5,  3),
                     []( 7,  8, 3,  9,  7),
                     []( 2,  4, 6, 10,  1))).


% 
% Winston "Operations Research", page 398, swimming team example
% (original version)
% See http://www.hakank.org/minizinc/assignment2.mzn 
% 
cost(2, minimize, []([](54, 54, 51, 53), 
                     [](51, 57, 52, 52),
                     [](50, 53, 54, 56),
                     [](56, 54, 55, 53))).


% 
% Winston "Operations Research", page 398, swimming team example
% See http://www.hakank.org/minizinc/assignment2_2.mzn 
% expanded version
%
cost(3, minimize, []([](54, 54, 51, 53,   50,60,70,80,90,100), 
                     [](51, 57, 52, 52,   40,50,60,70,80, 90),
                     [](50, 53, 54, 56,   40,50,60,80,93, 69),
                     [](56, 54, 55, 53,   60,80,40,60,50,100))).


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
cost(4, maximize, [](
                        [](7, 5, 8, 2),
                        [](7, 8, 9, 4),
                        [](3, 5, 7, 9),
                        [](5, 5, 6, 7))).


% From
%  "SAS OR 9.1 User's Guide Mathematical Programming"
% """
% Consider assigning five programmers to five programming jobs. Each
% programmer prefers specific programming job over others. [...] 
% Suppose you ask each programmer to rank the jobs according to preference
% (using 1 for the most preferred job and 5 for the least preffered job).
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
cost(5, minimize, []([](4, 1, 3, 5, 2),
                     [](2, 1, 3, 4, 5),
                     [](3, 2, 4, 1, 5),
                     [](2, 3, 4, 5, 1),
                     [](4, 2, 3, 1, 5))).



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
% (From Wikipedia, the free encyclopedia.) 
% """
% 
% """
% These data correspond to an example from [Christofides].
% """
%
% See http://www.hakank.org/minizinc/assignment6.mzn
%
cost(6,minimize, []([](13, 21, 20, 12,  8, 26, 22, 11),
                    [](12, 36, 25, 41, 40, 11,  4,  8),
                    [](35, 32, 13, 36, 26, 21, 13, 37),
                    [](34, 54,  7,  8, 12, 22, 11, 40),
                    [](21,  6, 45, 18, 24, 34, 12, 48),
                    [](42, 19, 39, 15, 14, 16, 28, 46),
                    [](16, 34, 38,  3, 34, 40, 22, 24),
                    [](26, 20,  5, 17, 45, 31, 37, 43))).

