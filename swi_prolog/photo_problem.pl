/*

  Photo problem in SWI Prolog

  Problem statement from Mozart/Oz tutorial:
  http://www.mozart-oz.org/home/doc/fdt/node37.html#section.reified.photo
  """
  Betty, Chris, Donald, Fred, Gary, Mary, and Paul want to align in one row for 
  taking a photo. Some of them have preferences next to whom they want to stand:
 
     1. Betty wants to stand next to Gary and Mary.
     2. Chris wants to stand next to Betty and Gary.
     3. Fred wants to stand next to Mary and Donald.
     4. Paul wants to stand next to Fred and Donald.
 
  Obviously, it is impossible to satisfy all preferences. Can you find an alignment that maximizes the number of satisfied preferences?
  """

  Oz solution: 
    6 # alignment(betty:5  chris:6  donald:1  fred:3  gary:7   mary:4   paul:2)
  [5, 6, 1, 3, 7, 4, 2]
  
   
  There are 8 solutions:
 
  positions = [3, 1, 6, 5, 2, 4, 7]
  positions = [3, 1, 7, 5, 2, 4, 6]
  positions = [3, 2, 6, 5, 1, 4, 7]
  positions = [3, 2, 7, 5, 1, 4, 6]
  positions = [5, 6, 1, 3, 7, 4, 2]  (the Oz solution.)
  positions = [5, 6, 2, 3, 7, 4, 1]
  positions = [5, 7, 1, 3, 6, 4, 2]
  positions = [5, 7, 2, 3, 6, 4, 1]



  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my SWI Prolog page: http://www.hakank.org/swi_prolog/

*/

:- use_module(library(clpfd)).
:- use_module(hakank_utils).

%%
%% Show optimal solution,
%%
go :-
        between(1,3,P),
        time(once(photo(P,Positions,Z))),
        writeln(z=Z),
        writeln(positions=Positions),
        nl,
        fail,
        nl.
go.

%%
%% Show all optimal solutions for problem 1.
%% (Problem 2 seems to be too big.)
%%
go2 :-
        time(once(photo(1,Positions,Z))),
        writeln(z=Z),
        writeln(positions=Positions),
        findall(Positions2,photo(1,Positions2,Z),L),
        maplist(writeln,L),
        length(L,Len),
        writeln(len=Len),
        nl.
       

photo(Problem,Positions,Z) :-

   format("Problem ~d\n",[Problem]),

   preferences(Problem, N, Preferences), % get the problem

   %% positions, decision variables
   length(Positions,N),
   Positions ins 1..N,

   all_different(Positions),
   maplist(check_preferences(Positions),Preferences,Scores),
   
   %% number of fullfilled preferences
   Z in 0..N,
   sum(Scores,#=,Z), 

   %% Z #>= 6, % for Problem 1
   %% Z #>= 12, % for Problem 2

   flatten([Positions,Scores],Vars),
   (var(Z)
   ->
    labeling([max(Z)],Vars)
   ;
    labeling([],Vars)
   ).

%%
%% Preferences: If P1 and P2 are beside each other,
%% then we score 1 point (otherwise 0)
%%
check_preferences(Positions,[Pref1,Pref2],Score) :-
        element(Pref1,Positions,P1),
        element(Pref2,Positions,P2),
        Score in 0..1,
        Score #=1  #<==> abs(P1-P2) #= 1.

%
% Problem 1 (see above):
% 1. Betty wants to stand next to Gary and Mary.
%     1 : 5, 6
% 2. Chris wants to stand next to Betty and Gary.
%     2 : 1, 5
% 3. Fred wants to stand next to Mary and Donald.
%     4 : 6, 3
% 4. Paul wants to stand next to Fred and Donald.
%     7 : 4, 3
%
% preferences(ProblemNumber, NumberOfPersons, Preferences)
preferences(1, N, Preferences) :- 
        N = 7,
        Preferences = 
        [[1,5],
         [1,6],
         [2,1],
         [2,5],
         [4,6],
         [4,3],
         [7,4],
         [7,3]].


% From http://www.g12.cs.mu.oz.au/minizinc/photo.data2
preferences(2, N, Preferences) :- 
        N = 11, 
        Preferences = 
        [[1,3], 
         [1,5], 
         [1,8], 
         [2,5], 
         [2,9], 
         [3,4], 
         [3,5], 
         [4,1], 
         [4,5], 
         [4,10],
         [5,6], 
         [5,1], 
         [6,1], 
         [6,9], 
         [7,3],
         [7,8], 
         [8,9],
         [8,7], 
         [9,10], 
         [10,11]].


% From http://www.ampl.com/NEW/LOGIC/EXAMPLES/photo9.dat
% (This seems to be a simplified version of problem #2)
preferences(3, N, Preferences) :- 
        N = 9, 
        Preferences = 
        [[1,3], 
         [1,5], 
         [1,8], 
         [2,5], 
         [2,9], 
         [3,4], 
         [3,5], 
         [4,1], 
         [4,5], 
         [5,1], 
         [5,6], 
         [6,1], 
         [6,9], 
         [7,3],
         [7,8], 
         [8,7],
         [8,9]].
