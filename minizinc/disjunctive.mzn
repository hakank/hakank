% 
% Global constraint disjunctive in MiniZinc.
% 
% From Global Constraint Catalogue
% http://www.emn.fr/x-info/sdemasse/gccat/Cdisjunctive.html
% """
% disjunctive​(TASKS)
% 
% Purpose
%
% All the tasks of the collection TASKS that have a duration strictly 
% greater than 0 should not overlap.
% 
% Example
% (
%  <
%   origin-1 duration-3,
%   origin-2 duration-0,
%   origin-7 duration-2,
%   origin-4 duration-1
%  >
% )
%
% Figure 4.98.1 shows the tasks with non-zero duration of the example. 
% Since these tasks do not overlap the disjunctive constraint holds.
%
%  --------    -----
%  | 1   |4|   | 3  |
%  ------------------------------------------->
%  1 2 3 4 5 6 7 8 9 10
%
% Figure 4.98.1. Tasks with non-zero duration
%

% """


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

int: n;
array[1..n] of var 1..10: origin;
array[1..n] of var 0..10: duration;
array[1..n] of var  0..1: used;
array[1..n] of var 0..10: end_time;

predicate disjunctive(array[int] of var int: v_origin, 
                      array[int] of var int: v_duration, 
                      array[int] of var int: v_end_time, 
                      array[int] of var int: v_used 
                      ) =
  assert(
        length(v_origin) == length(v_duration) /\
        length(v_duration) == length(v_end_time) /\
        length(v_end_time) == length(v_used),
        "All arrays must be of same length!")
  /\
  forall(i in index_set(v_origin)) (
     ( 
       v_duration[i] > 0 <-> (
          v_used[i] = 1 
          /\
          v_end_time[i] = v_origin[i] + v_duration[i]
          /\
          forall(j in index_set(v_origin) where i < j) (
            v_end_time[i] <= v_origin[j] % i left of j
            \/
            v_origin[i] >= v_end_time[j] % i right of j
          )     
       )
     )
     /\ % zero duration -> not used
     ( 
      v_duration[i] = 0 <-> (v_used[i] = 0 /\ v_end_time[i] = 0)

     )        
  )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) ( x[i] = y[i] ) )
; 

solve satisfy;

constraint
  cp1d(origin, [1,2,7,4])
  /\
  cp1d(duration, [3,0,2,1])

  /\
  disjunctive(origin, duration, end_time, used)

;

output [
  "used    : ", show(used), "\n",
  "origin  : ", show(origin), "\n",
  "duration: ", show(duration), "\n",
  "end_time: ", show(end_time), "\n",
];

%
% data
%
n = 4;

