/*

  Set covering deployment in SWI Prolog

  From http://mathworld.wolfram.com/SetCoveringDeployment.html
  """
  Set covering deployment (sometimes written "set-covering deployment"
  and abbreviated SCDP for "set covering deployment problem") seeks 
  an optimal stationing of troops in a set of regions so that a 
  relatively small number of troop units can control a large 
  geographic region. ReVelle and Rosing (2000) first described 
  this in a study of Emperor Constantine the Great's mobile field 
  army placements to secure the Roman Empire.
  """


  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).


% First find the optimal value (MinVal), 
% then find all the solutions with that value.
go :-
   
   writeln("\nFind the optimal solution\n"),
   problem(Matrix),
   armies(Armies),
   writeln(armies=Armies),
   once(set_covering_deployment(Matrix, Armies, MinVal,_Assignments)),

   format("\nNow find all optimal solutions with MinVal ~d:\n", MinVal),
   findall(Assignments, set_covering_deployment(Matrix, Armies, MinVal, Assignments),L), 
   length(L,Len),
   nl,
   writeln("All solutions:\n"),
   maplist(writeln, L),
   format("\nIt was ~d solution(s)\n", [Len]),
   nl.


set_covering_deployment(Matrix, Armies, MinVal, Assignments) :-

   % adjacency matrix of the cities, order N
   length(Matrix,N),

   % first army
   length(Xs,N),
   Xs ins 0..1,

   % second army
   length(Ys,N),
   Ys ins 0..1,

   %%
   %% Constraint 1: There is always an army in a city (+ maybe a backup)
   %%          Or rather: Is there a backup, there must be an
   %%          an army
   %% 
   maplist(constraint1,Xs,Ys),


   %%
   %% Constraint 2: There should always be an backup army near
   %% every city
   %%
   maplist(constraint2(Ys),Matrix,Xs),

           
   %% objective: minimize the total number of armies
   zip_sum(Xs,Ys,0,MinVal),

   % flatten([Xs,Ys],XYs), % variant
   % sum(XYs,#=,MinVal),

   %% either search for all solutions (for the minimum value) or
   %% the optimal value
   flatten([Xs,Ys,MinVal], Vars),
   (
    ground(MinVal)
   -> 
    labeling([],Vars)
   ;
    labeling([min(MinVal)], Vars)
   ),
  
   % convert X and Y to nicer representation
   assignments(Xs,Ys,Armies,Assignments),

   writeln(minVal=MinVal),
   writeln(x=Xs),
   writeln(y=Ys),
   writeln(assigments=Assignments),
   nl,nl.

%%
%% Constraint 1: There is always an army in a city (+ maybe a backup)
%%          Or rather: Is there a backup, there must be an
%%          an army
%% 
constraint1(X,Y) :- X #>= Y. 


%%
%% Constraint 2: There should always be an backup army near
%% every city
%%
constraint2(Ys,MatRow,X) :-
        scalar_product(MatRow,Ys,#=,YMSum),
        X + YMSum #>= 1.
          
%%      
%% zip sum
%%
zip_sum([],[],Sum,Sum).
zip_sum([X|Xs],[Y|Ys],Sum0,Sum) :-
        Sum1 #= Sum0 + X+Y,
        zip_sum(Xs,Ys,Sum1,Sum).

%%
%% Convert the assignments of Xs and Ys
%% to a nicer representation.
%%
assignments(Xs,Ys,Armies,Assignments) :-
        length(Ys, Len),
        findall((A=Num),(between(1,Len,I),
                   element(I,Xs,X),
                   element(I,Ys,Y),
                   Num #= X + Y, Num #> 0,
                   nth1(I,Armies,A)
                   ),
                Assignments).



problem(Problem) :- 
        Problem = 
        [[0, 1, 0, 1, 0, 0, 1, 1],
         [1, 0, 0, 1, 0, 0, 0, 0],
         [0, 0, 0, 0, 1, 1, 0, 0],
         [1, 1, 0, 0, 0, 0, 1, 0],
         [0, 0, 1, 0, 0, 1, 1, 0],
         [0, 0, 1, 0, 1, 0, 1, 1],
         [1, 0, 0, 1, 1, 1, 0, 1],
         [1, 0, 0, 0, 0, 1, 1, 0]].

armies(Armies) :- 
        Armies = ["Alexandria", "Asia Minor", "Britain", "Byzantium", "Gaul", "Iberia", "Rome", "Tunis"].
