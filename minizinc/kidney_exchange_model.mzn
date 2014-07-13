% 
% Kidney exchange in MiniZinc.
% 
% A simple model of kidney exchange, inspired by Pascal Van Hentenryck's
% introduction of the Coursera Course Discrete Optimization.
%
% The objective is to maximize the number of kidney exchanges given
% the compatible 
%
% Note that we are looking for cycles, which means that a person receiving
% a kidney must be able to give a kidney.
%

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: num_people;
array[1..num_people] of set of int: compatible;

% people that has no no potential donors (and can't get a kidney)
set of 1..num_people: non_compatible = { p | p in 1..num_people where card(compatible[p]) = 0 };

% The domains for each person
array[1..num_people] of set of int: 
   compatible_pruned = [ 
            if card(compatible[p]) = 0 then {p} else (compatible[p] diff non_compatible) union {p} endif
   | p in 1..num_people ];

%
% decision variables
%

% which kidney does person p get (or p if he/she gets no kidney)
array[1..num_people] of var 1..num_people: x;

var 0..num_people: z = sum([bool2int(x[i] != i) | i in 1..num_people]);

% solve satisfy;
% solve maximize z;
solve :: int_search(
      x,
      first_fail, 
      indomain_median,
      complete) 
    maximize z;

% Just allow the compatible people (+ p) in the domains
% and remove uncompatible people.
constraint
  forall(p in 1..num_people) (
     x[p] in compatible_pruned[p]    
  )
  /\
  alldifferent(x)

  % experimental: first test if all are donors (-> circuit)
  % 
  % works in MiniZinc v2.0, at least for some solvers.
  /\ (circuit(x) \/ true)
;

output [
  "z: " ++ show(z) ++ "\n" ++
  "x: " ++ show(x) ++ "\n" 
]
++
[
  "person: donor\n"
]
++
[
  if fix(x[i] = i) then 
     show_int(3, i) ++ ":   -\n"
  else 
     show_int(3, i) ++ ": " ++  show_int(3, x[i]) ++ "\n"
  endif
  | i in 1..num_people
]
++
[
  show(x) ++ "\n"
  ++ "z: " ++ show(z) ++ "\n" 
];


%
% data
%

% The compatibility matrix 
% (from Pascal's introduction lecture)
% who can give a kidney to person p
% This is a directed graph
% num_people = 8;
% compatible = 
%   [
%     {2,3}, % 1
%     {1,6}, % 2
%     {1,4,7}, % 3
%     {2}, % 4  
%     {2}, % 5
%     {5}, % 6
%     {8}, % 7
%     {3}, % 8
%   ];
