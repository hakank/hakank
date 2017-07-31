/*

  Photo problem in ECLiPSe.

  
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
  
   
  fz and ecl gives the following 8 solutions:
 
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
 

  Model created by Hakan Kjellerstrand, hakank@bonetmail.com
  See also my ECLiPSe page: http://www.hakank.org/eclipse/

*/

:-lib(ic).
:-lib(ic_global).
:-lib(ic_global_gac).
:-lib(ic_search).
:-lib(branch_and_bound).
:-lib(propia).
:-lib(viewable).

go :- 
        photo(1).

go2 :-
        photo(2).

photo(Problem) :-

        preferences(Problem, N, Preferences), % get the problem
        writeln(n:N),
        % preferences(Problem, Preferences), % get the problem
        % dim(Preferences,[N,N]), % for the matrix form
        dim(Preferences,[NumPreferences,2]), 
        writeln(num_preferences:NumPreferences),

        % positions, decision variables
        dim(Positions, [N]),
        Positions :: 1..N,

        viewable_create(photo, Positions),

        ic:alldifferent(Positions),
        % ic_global:alldifferent(Positions),
        % ic_global_gac:alldifferent(Positions),

        length(Diffs, NumPreferences),
        (for(I,1,NumPreferences), 
         fromto(Diffs,OutD,InD,[]), % collect the refications
         param(Preferences,Positions) do
             P1 #= Positions[Preferences[I,1]],
             P2 #= Positions[Preferences[I,2]],
             Diff #= P1-P2,
             % reify the condition that abs(difference) should be 1
             Reif #= (Diff #= 1 or Diff #= -1),
             OutD = [Reif|InD]
        ),
       
        % this is for using the matrix form of preferences
%         ( multifor([I,J],1,N),
%           % fromto(0, In, Out, Z),
%           fromto(Diffs,OutD,InD,[]), % collect the refications
%           param(Preferences, Positions) do
%               (
%                   I \= J,
%                   Preferences[I,J] > 0 
%               )
%               -> 
%               PI #= Positions[I],
%               PJ #= Positions[J],
%               Diff #= PI-PJ,
%               Reif #= (Diff #= 1 or Diff #= -1),
%               OutD = [Reif|InD]
%         ; 
%               OutD = InD
%         ),

        flatten_array(Preferences, PreferencesFlattened),
        NumPrefs #= sum(PreferencesFlattened),
        Z :: 0..NumPrefs, % number of fullfilled preferences
        Z #= sum(Diffs),

        % Z #>= 6, % for Problem 1
        % Z #>= 12, % for Problem 2

        flatten_array(Positions, Vars),
        % search(Vars,0,first_fail,indomain,complete,[]),
        ZNeg #= -Z,
        % minimize(search(Vars,0,occurrence,indomain_min,complete,[backtrack(Backtracks)]),ZNeg),
        minimize(search(Vars,0,first_fail,indomain_max,credit(N,bbs(5)),[backtrack(Backtracks)]),ZNeg),
        writeln("positions":Positions),
        writeln(backtracks:Backtracks),
        % writeln("diffs     ":Diffs),
        writeln(z:Z)
        ,viewable_expand(photo, 1, Positions, "positions").


% Problem 1 (see above):
% 1. Betty wants to stand next to Gary and Mary.
%     1 : 5, 6
% 2. Chris wants to stand next to Betty and Gary.
%     2 : 1, 5
% 3. Fred wants to stand next to Mary and Donald.
%     4 : 6, 3
% 4. Paul wants to stand next to Fred and Donald.
%     7 : 4, 3


% matrix approach of the preferences (not used)
% preferences(1, []([](0,0,0,0,1,1,0),   % Betty  1
%                   [](1,0,0,0,1,0,0),   % Chris  2
%                   [](0,0,0,0,0,0,0),   % Donald 3
%                   [](0,0,1,0,0,1,0),   % Fred   4
%                   [](0,0,0,0,0,0,0),   % Gary   5
%                   [](0,0,0,0,0,0,0),   % Mary   6
%                   [](0,0,1,1,0,0,0))). % Paul   7

%
% preferences(ProblemNumber, NumberOfPersons, Preferences)
%
preferences(1, 7, [](
                     [](1,5),
                     [](1,6),
                     [](2,1),
                     [](2,5),
                     [](4,6),
                     [](4,3),
                     [](7,4),
                     [](7,3))).


%                   
% From http://www.g12.cs.mu.oz.au/minizinc/photo.data2
% Though this is 1 based.
%
preferences(2, 11, [](
                         [](1,3), 
                         [](1,5), 
                         [](1,8), 
                         [](2,5), 
                         [](2,9), 
                         [](3,4), 
                         [](3,5), 
                         [](4,1), 
                         [](4,5), 
                         [](4,10),
                         [](5,6), 
                         [](5,1), 
                         [](6,1), 
                         [](6,9), 
                         [](7,3),
                         [](7,8), 
                         [](8,9),
                         [](8,7), 
                         [](9,10), 
                         [](10,11))).

