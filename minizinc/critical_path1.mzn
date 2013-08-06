%
% Critical Path in Minizinc

% Problem and model from 
% Winston "Introduction to Operations Research", page 441.
%
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: num_nodes = 6;
array[1..num_nodes] of 1..num_nodes: nodes = [1,2,3,4,5,6];

int: num_arcs = 7;
array[1..num_arcs,1..2] of 1..num_nodes: arcs;

array[1..num_arcs] of int: dur;
array[1..num_nodes] of var int: timex;

% sum of durations
var int: sum_dur = sum(i in 1..num_arcs) (dur[i]);

% difference between last and first time
solve minimize timex[6]-timex[1];

constraint 
   forall(i in 1..num_nodes) (
       timex[i] >= 0 /\
       timex[i] <= sum_dur
   )
   /\ % calculate the start times
   forall(i in 1..num_arcs) (
        timex[arcs[i,2]] >= timex[arcs[i,1]] + dur[i]
   )
;

output [
   "Total time: ", show(sum_dur), "\n",
   "Start times: ", show(timex),"\n",

];

%
% data
%

% the arcs (dependencies between projects)
arcs = array2d(1..num_arcs, 1..2, [
  1,2, 
  1,3,
  2,3, 
  3,4, 
  3,5, 
  4,5, 
  5,6
]) 
;

% durations
dur = [9,6,0,7,8,10,12];
