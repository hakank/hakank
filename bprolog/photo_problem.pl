/*

  Photo problem in B-Prolog.

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
 

  Model created by Hakan Kjellerstrand, hakank@gmail.com
  See also my B-Prolog page: http://www.hakank.org/bprolog/

*/

time2(Goal):-
        cputime(Start),
        statistics(backtracks, Backtracks1),
        call(Goal),
        statistics(backtracks, Backtracks2),
        cputime(End),
        T is (End-Start)/1000,
        Backtracks is Backtracks2 - Backtracks1,
        format('CPU time ~w seconds. Backtracks: ~d\n', [T, Backtracks]).

go :- 
        time2(photo(1)),
        nl,
        time2(photo(2)),
        nl.

photo(Problem) :-

        format("Problem ~d\n",[Problem]),

        preferences(Problem, N, Preferences), % get the problem
        % write(n:N),nl,

        % positions, decision variables
        length(Positions, N),
        Positions :: 1..N,

        % constraints
        all_different(Positions),
        foreach([Pref1,Pref2] in Preferences,
                ac(Diffs,[]), [P1,P2,Diff],
                (
                    element(Pref1,Positions,P1),
                    element(Pref2,Positions,P2),
                    Diff #= (abs(P1-P2) #= 1),
                    Diffs^1 = [Diff|Diffs^0]
                )
               ),
       
        % number of fullfilled preferences
        Z #= sum(Diffs), 

        % Z #>= 6, % for Problem 1
        % Z #>= 12, % for Problem 2

        term_variables([Positions,Diffs], Vars),
        labeling([leftmost,maximize(Z)],Vars),

        writeln(z:Z),
        writeln('positions':Positions),
        nl.


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


