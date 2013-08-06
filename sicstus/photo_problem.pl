/*

  Photo problem in SICStus Prolog.

  Problem statement from Mozart/Oz tutorial:
  http://www.mozart-oz.org/home/doc/fdt/node37.html#section.reified.photo
  """
  Betty, Chris, Donald, Fred, Gary, Mary, and Paul want to align in one row for taking a photo. Some of them have preferences next to whom they want to stand:
 
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
 

  Compare with the following models:
  * MiniZinc: http://www.hakank.org/minizinc/photo_hkj.mzn
  * Comet   : http://www.hakank.org/comet/photo_hkj.co
  * ECLiPSe : http://www.hakank.org/eclipse/photo_problem.ecl 


  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my SICStus Prolog page: http://www.hakank.org/sicstus/

*/

:-use_module(library(clpfd)).
:-use_module(library(lists)).

go :- 
        photo(1),nl,fd_statistics,
        nl,
        photo(2),nl,fd_statistics.

photo(Problem) :-

        preferences(Problem, N, Preferences), % get the problem
        write(n:N),nl,

        matrix(Preferences,[NumPreferences,2]), 
        write(num_preferences:NumPreferences),nl,

        % positions, decision variables
        length(Positions, N),
        domain(Positions,1,N),

        all_different(Positions),

        length(Diffs, NumPreferences),
        ( foreach([PreferencesI1,PreferencesI2],Preferences), 
          fromto(Diffs,OutD,InD,[]), % collect the refications
          param(Positions) do
              element(PreferencesI1,Positions, P1),
              element(PreferencesI2,Positions, P2),
              Diff #= P1-P2,
              % reify the condition that abs(difference) should be 1
              Reif in 0..1,
              Reif #= 1 #<=> (Diff #= 1 #\/ Diff #= -1),
              OutD = [Reif|InD]
        ),
        
        % append(Preferences, PreferencesFlattened),
        % sum(PreferencesFlattened,#=,NumPrefs),
        sum(Diffs,#=,Z), % number of fullfilled preferences

        % Z #>= 6, % for Problem 1
        % Z #>= 12, % for Problem 2

        labeling([ff,bisect,up,maximize(Z)],Positions),
        % labeling([ff,bisect,up],Positions),

        % write('NumPrefs':NumPrefs),nl,
        write('positions':Positions),nl,
        write(z:Z),nl.


matrix_element(X, I, J, Val) :-
        nth1(I, X, Row),
        element(J, Row, Val).


% From Mats Carlsson
matrix(_, []) :- !.
matrix(L, [Dim|Dims]) :-
        length(L, Dim),
        (   foreach(X,L),
            param(Dims)
        do  matrix(X, Dims)
        ).


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
preferences(1, 7, [[1,5],
                   [1,6],
                   [2,1],
                   [2,5],
                   [4,6],
                   [4,3],
                   [7,4],
                   [7,3]]).


% From http://www.g12.cs.mu.oz.au/minizinc/photo.data2
preferences(2, 11, [[1,3], 
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
                    [10,11]]).

