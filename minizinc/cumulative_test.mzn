% 
% Test of global constraint cumulative in MiniZinc.
%
% cumulative is defined in globals.mzn (in the MiniZinc package).
%  
% Example from Global constraints catalogue
% https://sofdem.github.io/gccat/gccat/Ccumulative.html
% """
% cumulative​(TASKS,​LIMIT)
% 
% Purpose
% 
% Cumulative scheduling constraint or scheduling under resource constraints. 
% Consider a set T of tasks described by the TASKS collection. The cumulative 
% constraint enforces that at each point in time, the cumulated height of 
% the set of tasks that overlap that point, does not exceed a given limit. 
% It also imposes for each task of T the constraint origin+duration=end.
% 
% Example
%    (
%    <
%     origin-1  duration-3  end-4   height-1,
%     origin-2  duration-9  end-11  height-2,
%     origin-3  duration-10 end-13  height-1,
%     origin-6  duration-6  end-12  height-1,
%     origin-7  duration-2  end-9   height-3
%     >,8
%     )
%
% Figure 4.71.1 [see the web page] shows the cumulated profile associated with 
% the example. To each task of the cumulative constraint corresponds a set of 
% rectangles coloured with the same colour: the sum of the lengths of the 
% rectangles corresponds to the duration of the task, while the height of the 
% rectangles (i.e., all the rectangles associated with a task have the same 
% height) corresponds to the resource consumption of the task. The cumulative 
% constraint holds since at each point in time we don't have a cumulated 
% resource consumption strictly greater than the upper limit 8 enforced by 
% the last argument of the cumulative constraint.
% """
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 5;
array[1..n] of var 1..20: origin;
array[1..n] of var 1..20: duration;
array[1..n] of var 1..20: height;
array[1..n] of var 1..20: end;
var 0..10: limitx; % = 7;  % note: the minimum limit is 7

% solve satisfy;
% solve minimize limitx;
solve minimize max(end);

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) ( x[i] = y[i] ) )
; 


constraint
  % origin = [1,2,3,6,7]
  duration = [3,9,10, 6,2] /\
  % end = [4,11,13,12,9]
  height = [1,2,1,1,3] /\

  % Note: If we use cumulative from globals.mzn we must handle 
  % end = origin + duration by ourselves
  cumulative(origin, duration, height,limitx)
  /\
  forall(i in 1..n) (
    end[i] = origin[i] + duration[i]
  )
;

output [
  "limit   : ", show(limitx), "\n",
  "origin  : ", show(origin), "\n",
  "duration: ", show(duration), "\n",
  "height  : ", show(height), "\n",
  "end     : ", show(end), "\n",

];
