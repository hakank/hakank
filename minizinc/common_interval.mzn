% 
% Global constraint common_interval in MiniZinc.
% 
% From Global Constraint Catalog:
% http://www.emn.fr/x-info/sdemasse/gccat/Ccommon_interval.html
% """
% common_interval (NCOMMON1, NCOMMON2, VARIABLES1, VARIABLES2, SIZE_INTERVAL) 
%
% Purpose
%
% NCOMMON1 is the number of variables of the collection of variables 
% VARIABLES1 taking a value in one of the intervals derived from the values
% assigned to the variables of the collection VARIABLES2: To each value v 
% assigned to a variable of the collection VARIABLES2 we associate the 
% interval [SIZE_INTERVAL*[v/SIZE_INTERVAL],
% SIZE_INTERVAL*[v/SIZE_INTERVAL]+SIZE_INTERVAL-1] .
%
% NCOMMON2 is the number of variables of the collection of variables 
% VARIABLES2 taking a value in one of the intervals derived from the 
% values assigned to the variables of the collection VARIABLES1: To each 
% value v assigned to a variable of the collection VARIABLES1 we associate 
% the interval  [SIZE_INTERVAL*[v/SIZE_INTERVAL],
% SIZE_INTERVAL*[v/SIZE_INTERVAL]+SIZE_INTERVAL-1].
%
% Example
%     (
%     3, 2, <8, 6, 6, 0>, 
%     <7, 3, 3, 3, 3, 7>, 3
%     )
%
% In the example, the last argument SIZE_INTERVAL=3 defines the following 
% family of intervals  [3*k, 3*k+2] , where k is an integer. 
% As a consequence the items of collection <8, 6, 6, 0> respectively 
% correspond to intervals [6, 8], [6, 8], [6, 8] and [0, 2].
% Similarly the items of collection <7, 3, 3, 3, 3, 7> respectively 
% correspond to intervals [6, 8], [3, 5], [3, 5], [3, 5], [3, 5], [6, 8].
% The common_interval constraint holds since:
%
%  * Its first argument NCOMMON1=3 is the number of intervals associated 
%    with the items of collection <8, 6, 6, 0> that also correspond to 
%    intervals associated with <7, 3, 3, 3, 3, 7>.
%  * Its second argument NCOMMON2=2 is the number of intervals associated 
%    with the items of collection <7, 3, 3, 3, 3, 7> that also correspond 
%    to intervals associated with <8, 6, 6, 0>.
% """
% 

%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

array[1..4] of var 0..9: x;
array[1..6] of var 0..9: y;
int: interval_size = 3;

var int: a;
var int: b;


solve satisfy;

% create the interval
predicate make_interval(array[int, int] of var int: intervals, int: interval_size, int: int_size) =
   intervals[1,1] = 0 /\
   intervals[1,2] = interval_size - 1 /\
   forall(i in 2..int_size) (
      intervals[i,1] = interval_size*(i-1) /\
      intervals[i,2] = (interval_size*i)-1
   )
;

% 
predicate count_interval(array[int] of var int: x, array[int,int] of var int: intervals, array[int] of var int: counts, int: int_size) =
  forall(j in 1..int_size) (
     counts[j] = sum(i in index_set(x)) (
                                         bool2int(x[i] >= intervals[j,1] /\
                                                  x[i] <= intervals[j,2])
                                        )
   )
 
;

predicate common_interval(var int: a, var int: b, array[int] of var int: x, array[int] of var int: y, int: interval_size) = 
   let {
     int: lbx = min(index_set(x)),
     int: ubx = max(index_set(x)),
     int: int_size_x = (ub_array(x) div interval_size)+1,
     array[1..int_size_x, 1..2] of var 0..ub_array(x)*2: x_intervals,
     array[1..int_size_x] of var 0..ubx: x_counts,
     int: lby = min(index_set(y)),
     int: uby = max(index_set(y)),
     int: int_size_y = (ub_array(y) div interval_size)+1,
     array[1..int_size_y, 1..2] of var 0..ub_array(y)*2: y_intervals,
     array[1..int_size_y] of var 0..uby: y_counts
   }
   in

   make_interval(x_intervals, interval_size, int_size_x)  /\
   make_interval(y_intervals, interval_size, int_size_y)   
   /\
   count_interval(x, x_intervals, x_counts,  int_size_x) /\
   count_interval(y, y_intervals, y_counts,  int_size_y) 

   /\ % this use the same principle as in 
      % http://www.hakank.org/minizinc/common_partition.mzn
   a = sum(i in index_set(x_counts)) (
           x_counts[i]*bool2int(y_counts[i] > 0)
          )
   /\
   b = sum(j in index_set(y_counts) ) (
          y_counts[j]*bool2int(x_counts[j] > 0)
          )
;

predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) ( x[i] = y[i] ) )
; 


constraint
   cp1d([8,6,6,7], x) /\
   cp1d([7,3,3,3,3,7],y) /\

   common_interval(a, b, x, y, interval_size)
;

output
[
  "a: " ++ show(a) ++ "\n" ++
  "b: " ++ show(b) ++ "\n" ++
  "x: " ++ show(x) ++ "\n" ++
  "y: " ++ show(y) ++ "\n" 
];