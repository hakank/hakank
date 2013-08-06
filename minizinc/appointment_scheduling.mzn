% 
% Appointment scheduling in MiniZinc.
% 
% From Stack Overflow
% Appointment scheduling algorithm (N people with N free-busy slots, constraint-satisfaction)
% http://stackoverflow.com/questions/11143439/appointment-scheduling-algorithm-n-people-with-n-free-busy-slots-constraint-sa
% ""
% Problem statement
%
% We have one employer that wants to interview N people, and therefore makes N 
% interview slots. Every person has a free-busy schedule for those slots. Give an algorithm 
% that schedules the N people into N slots if possible, and return a flag / error / etc if 
% it is impossible. What is the fastest possible runtime complexity?
% 
% My approaches so far
% 
% Naive: there are N! ways to schedule N people. Go through all of them, for each permutation, 
% check if it's feasible. O( n! )
% 
% Backtracking:
% 
%     Look for any interview time slots that can only have 1 person. Schedule the person, 
%     remove them from the list of candidates and remove the slot.
%     Look for any candidates that can only go into 1 slot. Schedule the person, remove them 
%     from the list of candidates and remove the slot.
%     Repeat 1 & 2 until there are no more combinations like that.
%     Pick a person, schedule them randomly into one of their available slots. Remember 
%     this operation.
%     Repeat 1, 2, 3 until we have a schedule or there is an unresolvable conflict. If we have a 
%     schedule, return it. If there's an unresolvable conflict, backtrack.
% 
% This is O( n! ) worst case, I think - which isn't any better.
% 
% There might be a D.P. solution as well - but I'm not seeing it yet.
% 
% Other thoughts
% 
% The problem can be represented in an NxN matrix, where the rows are "slots", columns are 
% "people", and the values are "1" for free and "0" for busy. Then, we're looking for a 
% row-column-swapped Identity Matrix within this matrix. Steps 1 & 2 are looking for a row or a 
% column with only one "1". (If the rank of the matrix is = N, I that means that there is a 
% solution. But the opposite does not hold) Another way to look at it is to treat the matrix 
% as an unweighed directed graph edge matrix. Then, the nodes each represent 1 candidate and 1 
% slot. We're then looking for a set of edges so that every node in the graph has one outgoing 
% edge and one incoming edge. Or, with 2x nodes, it would be a bipartite graph.
% 
% Example of a matrix:
% 
% 1 1 1 1
% 0 1 1 0
% 1 0 0 1
% 1 0 0 1
% 
% I have a feeling that this might be NP-C.
% ""
% 
% This is a matrix based approach.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 
int: n = 4;

% rows are time slots
% columns are people
array[1..n, 1..n] of int: m = array2d(1..n, 1..n,
[
1, 1, 1, 1,
0, 1, 1, 0,
1, 0, 0, 1,
1, 0, 0, 1,
]);

% decision variables
% 
% the assignment of persons to a slot (appointment number 1..n)
array[1..n, 1..n] of var 0..1: x;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

% 
% Matrix approach
% 
constraint
  forall(i in 1..n) (
    % ensure a free slot
    sum([m[i,j]*x[i,j] | j in 1..n]) = 1

    /\ % ensure one assignment per slot
    sum([x[i,j] | j in 1..n]) = 1
    /\
    sum([x[j,i] | j in 1..n]) = 1
  )
;

output 
[
  if j = 1 then "\n" else " " endif ++
    show(x[i,j])
  | i,j in 1..n
]
++
[
  if fix(x[i,j]) = 1 then "\nslot " ++ show(i) ++ ": " ++ show(j) else "" endif
  | i,j in 1..n
]
;

