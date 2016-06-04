% 
% TSPTW (TSP with Time Window) in MiniZinc.
% 
%   From or-tools User's Manual 
% "The Travelling Salesman Problem with Time Windows (TSPTW)"
% https://or-tools.googlecode.com/svn/trunk/documentation/user_manual/manual/tsp/tsptw.html
% """
% The Travelling Salesman Problem with Time Windows is similar to the TSP except that 
% cities (or clients) must be visited within a given time window. This added time 
% constraint - although it restricts the search tree[1] - renders the problem even 
% more difficult in practice!
% """

% This model of TSPTW are the dual view of:
% - a traditional TSP approach
%   This handles the circuit part, see http://hakank.org/minizinc/tsp_circuit.mzn
% - and then the path view (via circuit_path/2) which makes
%   it possible to create the cumulative times in order to
%   enforce that the visiting times for a city is within the appropriate
%   time windows, perhaps via the help of a waiting time.
%
% The model can handle both minimizing the distance of the tour, or the 
% time it takes (including the waiting times).

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n;
array[1..n,1..n] of int: dist;
int: min_val = min([dist[i,j] | i,j in 1..n where dist[i,j] > 0]);
int: max_val = max([dist[i,j] | i,j in 1..n]);

array[1..n,1..2] of int: time_windows;
int: max_time_window = max([time_windows[i,2] | i in 1..n]);
int: max_time_window_diff = max([time_windows[i,2]- time_windows[i,1]| i in 2..n]);

% decision variables

% Traditional TSP part
array[1..n] of var 1..n: x; % the circuit
array[1..n] of var min_val..max_val: d;
var int: travel_distance = sum(d); % alternative objective

% Time windows
array[1..n] of var 1..n: path; % circuit -> path (for cumulative reasoning)
array[1..n] of var 0..max_time_window: cum_times;
% array[1..n] of var 0..max_time_window: times2;
array[1..n] of var 0..max_time_window_diff: wait_time;
var int: travel_time = cum_times[n]; % objective
var 0..max_time_window: total_wait_times = sum(wait_time);

% solve satisfy;
% solve minimize travel_distance;
% solve minimize travel_time;
% solve :: int_search(cum_times, first_fail, indomain_min, complete) minimize cum_times[n]; % travel_distance;
% solve :: int_search(d, max_regret, indomain_split, complete) minimize travel_time;

% solve :: int_search(cum_times, first_fail, indomain_split, complete) satisfy;

% this labeling is often better for traditional CP solvers (and some others)
% fz: 17.5s(!)
% picat_cp: 0.266s (!)
% chuffed: 0.255s
% lazy: 0.874s
% sicstus: 0.627s
% picat_sat: 20.6s
% jacop: >30s
% cpx2: 2.329s (--free 1.74s)
% izplus: 3.498s
% g12_fd: >30s
% minicsp: 3.192s
% smt: 9.237s
% smt_java: 12.057s
% ECLiPSe/ic: 14.402s
% ECLiPSe/fd: error
solve :: int_search(d ++ path, first_fail, indomain_split, complete) minimize travel_time;

% p starts with x[1]
predicate circuit_path(array[int] of var int: x, 
                       array[int] of var int: p) =
  let {
    int: len = length(x)
  } in
  % we always starts the path at 1
  p[1] = x[1]
  /\
  p[len] = 1
  /\
  forall(i in 2..len) (
    p[i] = x[p[i-1]] 
  )
; 


constraint

    % Standard TSP 
    all_different(x) /\ % this sometimes boosts performance
    circuit(x)
    /\
    forall(i in 1..n) (
      d[i] = dist[i,x[i]] 
    )

    % Time windows (dual view)
    /\
    all_different(path) % this sometimes boosts performance
    /\ 
    circuit_path(x,path) % connect circuit and path

    % accumulate the times
    %/\
    % times2[1] = dist[path[n],path[1]] % from 1 -> first visited city
    /\ 
    cum_times[1] = dist[path[n],path[1]] % times2[1] 
    % /\
    % cum_times2[1] = dist[1,x[1]]
    /\
    forall(i in 2..n) (
      % times2[i] = dist[path[i-1],path[i]] + wait_time[path[i]] /\
      % cum_times[i] = cum_times[i-1] + times2[i] 

      cum_times[i] = cum_times[i-1] + dist[path[i-1],path[i]] + wait_time[path[i]]

      % cum_times[i] = cum_times[i-1] + d[path[i-1]] + wait_time[path[i]]

      % /\
      % cum_times2[i] = cum_times2[i-1] + d[i] + wait_time[i]
    )
    /\ 
    increasing(cum_times) % redundant but may boost performance

    /\ 
    forall(i in 1..n) (
      %% some solvers don't like this (it's interpreted as a set constraint)
      % cum_times[i] in time_windows[path[i],1]..time_windows[path[i],2]

      cum_times[i] >= time_windows[path[i],1] /\
      cum_times[i] <= time_windows[path[i],2]
 
    )

    % one optimal solution with travel_distance 378 and travel_time 387
    % From https://or-tools.googlecode.com/svn/trunk/documentation/user_manual/manual/tsp/tsptw.html
    %% path = [1,17,10,20,18,19,11,6,16,2,12,13,7,14,8,3,5,9,21,4,15] % original
    %% Note: Here I placed 1 last (it was first in the above page)
    % /\ path = [17,10,20,18,19,11,6,16,2,12,13,7,14,8,3,5,9,21,4,15,1] % swapped 1<->n (as path/1)

    % /\ travel_time = 387
    % /\ travel_distance = 382
    % /\ total_wait_times = 5

;

output [
  "travel_distance: \(travel_distance)\n",
  "x; \(x)\n",
  "d: \(d)\n\n",
  "path     : \(path)\n",
  % "times2   : \(times2)\n",
  "cum_times: \(cum_times)\n",
  "wait_time: \(wait_time)\n",
  "travel_time: \(travel_time)\n",
  "total_wait_times: \(total_wait_times)\n",
];

% From https://or-tools.googlecode.com/svn/trunk/documentation/user_manual/manual/tsp/tsptw.html
% n20w20.001.txt
n = 21;
dist = array2d(1..n,1..n,
[
%1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21
 0,19,17,34, 7,20,10,17,28,15,23,29,23,29,21,20, 9,16,21,13,12, % 1
19, 0,10,41,26, 3,27,25,15,17,17,14,18,48,17, 6,21,14,17,13,31, % 2
17,10, 0,47,23,13,26,15,25,22,26,24,27,44, 7, 5,23,21,25,18,29, % 3
34,41,47, 0,36,39,25,51,36,24,27,38,25,44,54,45,25,28,26,28,27, % 4
 7,26,23,36, 0,27,11,17,35,22,30,36,30,22,25,26,14,23,28,20,10, % 5
20, 3,13,39,27, 0,26,27,12,15,14,11,15,49,20, 9,20,11,14,11,30, % 6
10,27,26,25,11,26, 0,26,31,14,23,32,22,25,31,28, 6,17,21,15, 4, % 7
17,25,15,51,17,27,26, 0,39,31,38,38,38,34,13,20,26,31,36,28,27, % 8
28,15,25,36,35,12,31,39, 0,17, 9, 2,11,56,32,21,24,13,11,15,35, % 9
15,17,22,24,22,15,14,31,17, 0, 9,18, 8,39,29,21, 8, 4, 7, 4,18, % 10
23,17,26,27,30,14,23,38, 9, 9, 0,11, 2,48,33,23,17, 7, 2,10,27, % 11
29,14,24,38,36,11,32,38, 2,18,11, 0,13,57,31,20,25,14,13,17,36, % 12
23,18,27,25,30,15,22,38,11, 8, 2,13, 0,47,34,24,16, 7, 2,10,26, % 13
29,48,44,44,22,49,25,34,56,39,48,57,47, 0,46,48,31,42,46,40,21, % 14
21,17, 7,54,25,20,31,13,32,29,33,31,34,46, 0,11,29,28,32,25,33, % 15
20, 6, 5,45,26, 9,28,20,21,21,23,20,24,48,11, 0,23,19,22,17,32, % 16
 9,21,23,25,14,20, 6,26,24, 8,17,25,16,31,29,23, 0,11,15, 9,10, % 17
16,14,21,28,23,11,17,31,13, 4, 7,14, 7,42,28,19,11, 0, 5, 3,21, % 18
21,17,25,26,28,14,21,36,11, 7, 2,13, 2,46,32,22,15, 5, 0, 8,25, % 19
13,13,18,28,20,11,15,28,15, 4,10,17,10,40,25,17, 9, 3, 8, 0,19, % 20
12,31,29,27,10,30, 4,27,35,18,27,36,26,21,33,32,10,21,25,19, 0, % 21


]);

time_windows = array2d(1..n,1..2,
[ 0,       408,  % 1
 62,        68,  % 2
181,       205,  % 3
306,       324,  % 4
214,       217,  % 5
 51,        61,  % 6
102,       129,  % 7
175,       186,  % 8
250,       263,  % 9
  3,        23,  % 10
 21,        49,  % 11
 79,        90,  % 12
 78,        96,  % 13
140,       154,  % 14
354,       386,  % 15
 42,        63,  % 16
  2,        13,  % 17
 24,        42,  % 18
 20,        33,  % 19
  9,        21,  % 20
275,       300,  % 21
]);