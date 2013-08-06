% 
% Wed Mar 12 22:47:15 2008/hakank@bonetmail.com
% 
% Martin Gardner The Last Recreations, sid 121
% """
% A woman plans to invite 15 friends to dinner. For 35 days she
% wants to have dinner with exactly three friends a day, and she
%  wants to arrange the triplets so that each pair of friends will
% only come once. Is this arrangement possible?
% """
%
% (This is of course a Steiner triplet problem.)
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
% Minizinc gives the following solution:
% [ { 13, 14, 15 }, { 11, 12, 15 }, { 10, 12, 14 }, { 10, 11, 13 },
%  { 9, 12, 13 }, { 9, 11, 14 }, { 9, 10, 15 }, { 7, 8, 15 }, { 6, 8, 14 },
%  { 6, 7, 13 }, { 5, 8, 13 }, { 5, 7, 14 }, { 5, 6, 15 }, { 4, 8, 12 },
%  { 4, 7, 11 }, { 4, 6, 10 }, { 4, 5, 9 }, { 3, 8, 11 }, { 3, 7, 12 },
%  { 3, 6, 9 }, { 3, 5, 10 }, { 3, 4, 15 }, { 2, 8, 10 }, { 2, 7, 9 },
%  { 2, 6, 12 }, { 2, 5, 11 }, { 2, 4, 14 }, { 2, 3, 13 }, { 1, 8, 9 },
%  { 1, 7, 10 }, { 1, 6, 11 }, { 1, 5, 12 }, { 1, 4, 13 }, { 1, 3, 14 },
%  { 1, 2, 15 } ]
%

include "globals.mzn"; 
int: num_persons_per_meeting = 3;
int: num_days = 35;

set of int: persons = 1..15;
array[1..num_days] of var set of persons: days;

solve satisfy;
% solve :: set_search(days, input_order, indomain_min, complete) satisfy;

constraint
  all_different(days)  % different triplets (i.e. day)
  /\
  forall(i in 1..num_days) (
     card(days[i]) = num_persons_per_meeting
  )
  /\ % max 1 common person in each days
  forall(i,j in 1..num_days where i != j) (     
    card(days[i] intersect days[j]) <= 1
  )
;

output [
  show(days), "\n",
]


