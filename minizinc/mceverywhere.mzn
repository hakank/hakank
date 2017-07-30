% 
% McEverywhere problem in MiniZinc.
% 
% From PuzzlOR April 2012
% http://puzzlor.editme.com/McEverywhere
% http://www.informs.org/ORMS-Today/Private-Articles/April-Volume-39-Number-2/THE-PUZZLOR
% """
% Deciding how many fast food restaurants to build in a town takes careful 
% planning.  Building too many will result in wasted capital and building too 
% few will result in lost business.  
%  
% [
%   Figure 1 in matrix for where 1 indicates a household
%        
%    1 2 3 4 5 6 7 8 9 10
%    0,0,0,1,0,0,0,1,1,1, % 1
%    0,0,0,1,0,0,1,0,0,0, % 2
%    0,0,0,0,0,0,0,1,1,0, % 3
%    0,1,0,0,0,0,0,0,0,0, % 4
%    1,0,0,0,0,0,0,0,0,0, % 5
%    0,1,0,0,1,1,0,0,0,0, % 6
%    0,0,0,0,1,0,0,0,1,0, % 7
%    0,0,1,0,1,0,0,0,0,0, % 8
%    0,0,0,0,1,0,0,0,1,0, % 9
%    0,0,0,0,0,0,1,0,0,0, % 10
%  ]
% 
% The map in Figure 1 shows the locations of 20 homes in a small town.  Sadly, 
% there are no McEverywhere restaurants where the residents can eat.  As the 
% planner for McEverywhere corporation, you have been asked to build restaurants 
% so that no resident has to travel more than 4km to reach a restaurant.  
% You can build as many restaurants as you like and restaurants can be built on 
% any cell (including one that has a home on it).
% 
% Use a direct line between cells to calculate travel distance.  The distance 
% between two adjacent cells is 1km and the distance between two diagonal 
% cells is 1.41 km.
% 
% Question:  What is the minimum number of McEverywhere restaurants needed so 
% that no resident has to travel more than 4km to reach one?
% """

% Tentative solution:
%
%   X: both home and restaurant in the cell
%   H: only home
%   R: only restaurant
%

% Solutions for Gecode/fz
% 
% num_restaurants: 3
% 
% _ _ _ H _ _ _ H H H
% _ _ _ H _ _ H _ _ _
% _ _ _ _ _ _ _ H H _
% _ H _ _ _ _ _ _ _ R
% H _ _ R _ _ _ _ _ _
% _ H _ _ H H _ _ _ _
% _ _ _ _ H _ _ _ H _
% _ _ H _ H _ _ _ _ _
% _ _ _ _ H _ _ _ H _
% _ _ _ _ _ _ H R _ _
% 
% Num restaurants in the 'hood:
% _ _ _ 1 _ _ _ 1 1 1
% _ _ _ 1 _ _ 1 _ _ _
% _ _ _ _ _ _ _ 1 1 _
% _ 1 _ _ _ _ _ _ _ _
% 1 _ _ _ _ _ _ _ _ _
% _ 1 _ _ 1 1 _ _ _ _
% _ _ _ _ 1 _ _ _ 2 _
% _ _ 1 _ 2 _ _ _ _ _
% _ _ _ _ 1 _ _ _ 1 _
% _ _ _ _ _ _ 1 _ _ _
% ----------
% ==========
% %%  runtime:       0.030 (30.763 ms)
% %%  solvetime:     0.026 (26.930 ms)
% %%  solutions:     2
% %%  variables:     402
% %%  propagators:   241
% %%  propagations:  89851
% %%  nodes:         1874
% %%  failures:      894
% %%  peak depth:    98
% %%  peak memory:   1689 KB


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 
int: n = 10;
int: max_dist = 4*4; % square this since we don't do a sqrt in dist
array[1..n, 1..n] of int: homes;

% decision variables
array[1..n, 1..n] of var 0..1: x;

array[1..n, 1..n] of var 0..n*n: counts;

var 0..n*n: num_restaurants = sum(x);

% solve satisfy;
% solve minimize num_restaurants;
solve :: int_search(
        [x[i,j] | i,j in 1..n], %  ++ [counts[i,j] | i,j in 1..n], 
        first_fail, %  max_regret,
        indomain_min, 
        complete) 
    % satisfy;
    minimize num_restaurants;


% 
% Calculate the distance between two cells.
% 
% Note: Distance is not squared so we have to 
%       adjust the distance limit as well.
% 
predicate dist(var int: i1, var int: j1, 
               int: i2, int: j2, var int: d) =
      d = abs(i1-i2)*abs(i1-i2) +
            abs(j1-j2)*abs(j1-j2)
;


constraint

   % % slightly slower version.
   % forall(i, j in 1..n where homes[i,j] = 1) (
   %      let {
   %         var 1..10: ri, % index of the restaurant
   %         var 1..10: rj,
   %         var 0..max_dist: d   % the distance (squared so we don't 
   %                        % about sqrt in dist()
   %      } in 
   %      dist(ri,rj, i,j, d)
   %      /\
   %      x[ri,rj] = 1
   % )

   % This is slightly faster.
   % 
   % For each home, there must be one restaurant 
   % in the neighbourhood.
   forall(i, j in 1..n where homes[i,j] = 1) (
      exists(ri,rj in 1..n, d in 0..max_dist) (
         dist(ri,rj, i,j, d)
         /\
         x[ri,rj] = 1
      )
   )

   /\ % count the number of restaurants in the 'hood
      % of each home.
   forall(i, j in 1..n) (
     % if homes[i,j] = 1 then
         counts[i,j] = sum(ri,rj in 1..n, d in 0..max_dist) (
                          bool2int(dist(ri,rj, i,j, d)  /\ x[ri,rj] = 1)
                       )
     % else 
     %   counts[i,j] = 0
     %endif
   )


   % /\ % for solve satisfy
   % num_restaurants = 3

   % /\ % extra constraint, just for fun:
   %    % a home cannot be covered by more than 1 restaurant
   %    % (still needs just 3 restaurants)
   % forall(i,j in 1..n where homes[i,j] = 1) (
   %    counts[i,j] = 1   
   % ) 
;

output [
  "num_restaurants: " ++ show(num_restaurants) ++ "\n"
]
++
[
  if j = 1 then "\n" else " " endif ++
    if fix(x[i,j]) = 1 /\ homes[i,j] = 1 then 
       "X" % Restaurant in the same cell as a home
    elseif fix(x[i,j]) = 1 then
       "R" % just a restaurant
    elseif homes[i,j] = 1 then 
       "H" % Just a home
    else 
       "_"
    endif

  | i,j in 1..n
]
++
[ "\nNum restaurants in the 'hood:"]
++
[
  if j = 1 then "\n" else " " endif ++ 
    if homes[i,j] = 1 then
       show(counts[i,j])
    else 
       "_"
    endif
  | i, j in 1..n
]
 ++ ["\n"]
;


%
% data
%
homes = array2d(1..n, 1..n,
[
  %1 2 3 4 5 6 7 8 9 10
   0,0,0,1,0,0,0,1,1,1, % 1
   0,0,0,1,0,0,1,0,0,0, % 2
   0,0,0,0,0,0,0,1,1,0, % 3
   0,1,0,0,0,0,0,0,0,0, % 4
   1,0,0,0,0,0,0,0,0,0, % 5
   0,1,0,0,1,1,0,0,0,0, % 6
   0,0,0,0,1,0,0,0,1,0, % 7
   0,0,1,0,1,0,0,0,0,0, % 8
   0,0,0,0,1,0,0,0,1,0, % 9
   0,0,0,0,0,0,1,0,0,0, % 10
]);


