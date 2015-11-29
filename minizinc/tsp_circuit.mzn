%
% TSP with circuit predicate in Minizinc.
% 
% Model created by Hakan Kjellerstrand, hakank@gamil.com
% with help from Krzysztof Kuchcinski (of the JaCoP team)
%                 
% There are two optimal solutions (with distance=6859):
%
%   x: [14, 4, 2, 8, 11, 15, 6, 1, 10, 16, 9, 7, 12, 13, 5, 3]
%   6859
%   ----------
%   x: [8, 3, 16, 2, 15, 7, 12, 4, 11, 9, 5, 13, 14, 1, 6, 10]
%   6859
%
%
% Also, see my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn";
% include "jacop.mzn";

predicate circuit_me(array[int] of var int: x) =
  let { 
        int: lbx = 1,
        int: ubx = length(x),
        array[lbx..ubx] of var lbx..ubx: z
  }
  in
   all_different(x) :: domain /\
   all_different(z) :: domain /\

   % put the orbit of x[1] in in z[1..n]
   % 
   z[lbx] = x[lbx] /\ 
   forall(i in lbx+1..ubx) (
      z[i] = x[z[i-1]]
   )
   /\ % may not be 1 for i < n
   forall(i in lbx..ubx-1) (
      z[i] != lbx
   )
   /\  % when i = n it must be 1
   z[ubx] = lbx
;


%
% Data
%
int: n; % number of nodes

array[1..n] of var 1..n: x;             % the circuit
array[1..n, 1..n] of int: distances;  % distance matrix
int: min_val = min([distances[i,j] | i,j in 1..n where distances[i,j] > 0]);
int: max_val = max([distances[i,j] | i,j in 1..n]);
array[1..n] of var min_val..max_val: d;
var int: distance = sum(d);

% solve minimize distance;
solve :: int_search(d, max_regret, indomain_split, complete) minimize distance;

constraint
    all_different(x) :: domain /\ % this speeds up for some solvers

    circuit(x) % use this for solvers with built-in circuit predicate, e.g. JaCoP
               % or - lately - the built-in version in MiniZinc distribution

    % circuit_me(x) % hakank: use this version for solvers without the built-in circuit predicate

    /\
    forall(i in 1..n) (
      distances[i,x[i]] = d[i]
    )
;

% 
% constraint distance = 6859;
%


% 
% data
% 
n = 16;
distances = 
array2d(1..n, 1..n,
[
   0,  509,  501,  312, 1019,  736,  656,   60, 1039,  726, 2314,  479,  448,  479,  619,  150,
 509,    0,  126,  474, 1526, 1226, 1133,  532, 1449, 1122, 2789,  958,  941,  978, 1127,  542,
 501,  126,    0,  541, 1516, 1184, 1084,  536, 1371, 1045, 2728,  913,  904,  946, 1115,  499,
 312,  474,  541,    0, 1157,  980,  919,  271, 1333, 1029, 2553,  751,  704,  720,  783,  455,
1019, 1526, 1516, 1157,    0,  478,  583,  996,  858,  855, 1504,  677,  651,  600,  401, 1033,
 736, 1226, 1184,  980,  478,    0,  115,  740,  470,  379, 1581,  271,  289,  261,  308,  687,
 656, 1133, 1084,  919,  583,  115,    0,  667,  455,  288, 1661,  177,  216,  207,  343,  592,
  60,  532,  536,  271,  996,  740,  667,    0, 1066,  759, 2320,  493,  454,  479,  598,  206,
1039, 1449, 1371, 1333,  858,  470,  455, 1066,    0,  328, 1387,  591,  650,  656,  776,  933,
 726, 1122, 1045, 1029,  855,  379,  288,  759,  328,    0, 1697,  333,  400,  427,  622,  610,
2314, 2789, 2728, 2553, 1504, 1581, 1661, 2320, 1387, 1697,    0, 1838, 1868, 1841, 1789, 2248,
 479,  958,  913,  751,  677,  271,  177,  493,  591,  333, 1838,    0,   68,  105,  336,  417,
 448,  941,  904,  704,  651,  289,  216,  454,  650,  400, 1868,   68,    0,   52,  287,  406,
 479,  978,  946,  720,  600,  261,  207,  479,  656,  427, 1841,  105,   52,    0,  237,  449,
 619, 1127, 1115,  783,  401,  308,  343,  598,  776,  622, 1789,  336,  287,  237,    0,  636,
 150,  542,  499,  455, 1033,  687,  592,  206,  933,  610, 2248,  417,  406,  449,  636,    0
]);


output [
       "x: ", show(x),"\n",
       show(distance)
];
