% 
% The Athletics Committee problem in MiniZinc.
% 
% Puzzles that Solve Themselves — Peter Winkler
% https://www.youtube.com/watch?v=RQtCRpWvVuw&t=6s
% @ 2:03ff
% """
% Every professor wants to be on the Athlectics Committee -- free
% tickets to your favorite event!
% 
% To keep the committee from becoming too cliquish, the college forbids
% anyone from serving who has 3 or more friends on the committee.
% 
% That's OK, because if you have 3 or more friends on the committee, you
% get free tickets to your favorite event!
%
% Can the committee be chosen so that no one on it has 3 friends on the
% committee, but everyone _off_ the committee has 3 friends on it?
% """
% 
% Assumptions: 
%  - friends is a symmetric relation (i is a friend of j -> j is a friend of i)
%  - a person is not a friend of him/herself
%  - the last constraint means that everyone _off_ the committee has _exactly 3 friends
%    in it.
%  - there must be at least one person in the committe and one person outside it
%
% Number of solutions for n
% (Found by the Picat model: http://hakank.org/picat/the_athletic_committee.pi )
% 
% n        #sols      
% ----------------
%  1           0
%  2           0
%  3           0
%  4          32 
%  5         980
%  6       36140
%  7     2075360
%  8   206866912
%  9 37299417288
% 10

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 4; % total number of people

% decision variables
array[1..n, 1..n] of var 0..1: friends; % friends[i,j] = 1 -> i and j are friends (symmetrical)
array[1..n] of var 0..1: x; % x[i] = 1 -> i is on the committee

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  forall(i in 1..n) (
    if x[i] = 1 then 
       sum([friends[i,j]*x[j] | j in 1..n]) < 3 
    else 
       sum([friends[i,j]*x[j] | j in 1..n]) = 3 
    endif

    /\
    friends[i,i] = 0 % a person is not a friend of him/herself
    /\
    % symmetry of friends
    forall(j in 1..n) (
      friends[i,j] = friends[j,i]
    )
  )

  /\ sum(x) > 0 % at least one person is in the committee
  /\ sum([x[i] = 0 | i in 1..n]) > 0 % at least one person is outside the committee


  % symmetry breaking
  % /\ increasing(x) 
;

output 
[ if j = 1 then "\n" ++ show_int(2,i) ++ " (" ++ show(x[i]) ++ "): " else " " endif ++
  show(friends[i,j])
  | i,j in 1..n  
]
++
[
  "\nx: \(x)\n",
]
;

