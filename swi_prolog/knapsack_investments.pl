/*

  Knapsack (investment) problem in SWI Prolog

  From the Swedish book
  Lundgren, Ronnqvist, Varbrand: 
  "Optimeringslara" (Optimization theory),
  page 393ff.
  
  A company shall invest in some building projects with the following
  limits:
 
   - budget of 225 Mkr (million Swedish kronor)
   - 28 persons available
   - maximum 9 projects can be selected
   - some project may not be selected together with other projects, 
     and some projects must be selected together with other.
  
  (I'm keeping the Swedish object names.)
 
  No.  Object   Value(kkr) Budget(Mkr) Personell  Not with  Requires
  1  Ishall      600        35            5        10        -
  2  Sporthall   400        34            3        -         -
  3  Hotell      100        26            4        -         15
  4  Restaurang  150        12            2        -         15
  5  Kontor A     80        10            2        6         -
  6  Kontor B    120        18            2        5         -
  7  Skola       200        32            4        -         -
  8  Dagis       220        11            1        -         7
  9  Lager        90        10            1        -         -
  10 Simhall     380        22            5        1         -
  11 Hyreshus    290        27            3        15        -
  12 Bilverkstad 130        18            2        -         -
  13 Tennishall   80        16            2        -         2
  14 Idrottsanl. 270        29            4        -         2
  15 Båthamn     280        22            3        11        -
  
 
  Solution (page 395): 
  The following projects is selected
    1,2,4,6,7,8,12,14,15
  and optimal value is 2370kkr.
 

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

go :-

   num_projects(NumProjects), % number of projects to select from
   max_budget(MaxBudget),   % budget limit 
   max_persons(MaxPersons),  % persons available
   max_projects(MaxProjects), % max number of projects to select

   % the values of each project
   values(Values),
   budgets(Budgets),
   personell(Personell),

   % project i cannot be selected with project j
   not_with(NotWith),

   % project i requires project j
   requires(Requires),

   % decision variable: what project to select
   length(X,NumProjects),
   X ins 0..1,

   scalar_product(Personell,X,#=, TotalPersons),
   scalar_product(Budgets,X,#=, TotalBudget),
   sum(X,#=,TotalProjects),
   %% the objective to maximize
   scalar_product(Values,X,#=,TotalValues),

   %% TotalValues #>= 2370, % for checking unicity of solutions
   
   % resource limits
   TotalBudget   #=< MaxBudget,
   TotalPersons  #=< MaxPersons,
   TotalProjects #=< MaxProjects,

   %% projects that require other projects
   maplist(check_requires(X),Requires),
   
   %% projects excluding other projects
   maplist(excluding_projects(X),NotWith),

   %% search
   labeling([max(TotalValues),ff,down], X),

   %% output
   nl,
   write(X),nl,
   writeln("selected projects:"),
   findall(P,
           (between(1,NumProjects,P),
            element(P,X,1)
           ),
           Sol),
   writeln(Sol),
   
   nl,
   writeln(total_persons=TotalPersons),
   writeln(total_budget=TotalBudget),
   writeln(total_projects=TotalProjects),
   writeln(total_values=TotalValues),
   nl.


%% projects that require other projects
check_requires(X,[P1,P2]) :-
        element(P1,X,XP1),
        element(P2,X,XP2),
        XP1 #==> XP2.

%% projects excluding other projects
excluding_projects(X,[P1,P2]) :-
        element(P1,X,XP1),
        element(P2,X,XP2),
        XP1 #= 1 #==> XP2 #= 0.

%
% data
%
num_projects(N) :- N = 15.
max_budget(M) :- M = 225.
max_projects(M) :- M = 9.
max_persons(Max) :- Max = 28.

values(Values) :-
        Values = [600,400,100,150, 80,120,200,220, 90,380,290,130, 80,270,280].

budgets(Budgets) :-
        Budgets = [35,34,26,12,10,18,32,11,10,22,27,18,16,29,22].

not_with(NotWith) :- 
        NotWith = 
        [[1, 10],
         [5, 6],
         [6, 5],
         [10, 1],
         [11, 15],
         [15, 11]].

requires(Requires) :- 
        Requires = 
        [[3, 15],
         [4, 15],
         [8, 7],
         [13, 2],
         [14, 2]].
personell(Personell) :-
        Personell = [5,3,4,2,2,2,4,1,1,5,3,2,2,4,3].

