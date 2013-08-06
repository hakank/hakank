% Bus scheduling in Minizinc
%
% Problem from Taha "Introduction to Operations Research", page 58.
%
% This is a slightly more general model than Taha's.
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

int: time_slots; % number of time slots

% demand: minimum number of buses at time t
array[1..time_slots] of int: demands; 

% result array: how many buses start the schedule at time slot t
array[1..time_slots] of var 0..max(demands): x;

% the objective to minimize: the total number of buses
var int: z = sum(i in 1..time_slots) (x[i]);

% minimize the number of buses needed
solve :: int_search(x, first_fail, indomain_min, complete) minimize z;
% solve satisfy;

constraint
  % must be 0 or more buses at each time slot
  forall(i in 1..time_slots) (x[i] >= 0)

  /\ % meet the demands for this and the next time slot
  forall(i in 1..time_slots-1) (x[i]+x[i+1] >= demands[i])

  /\ % demand "around the clock"
  x[time_slots] + x[1] >= demands[time_slots]

%  /\ % for solve satisfy
%  sum(i in 1..time_slots) (x[i]) = 26
;

%
% data
%
time_slots = 6;
demands = [8, 10, 7, 12, 4, 4];

output
[
  "x: " ++ show(x) ++ "\n" ++
  "z: " ++ show(z) ++ "\n"
];