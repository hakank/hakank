% 
% Scheduling (Building a house) in MiniZinc.
% 
% 
%   This model is adapted OPL model sched_intro.mod (examples).
%   """
%   This is a basic problem that involves building a house. The masonry,
%   roofing, painting, etc.  must be scheduled. Some tasks must
%   necessarily take place before others, and these requirements are
%   expressed through precedence constraints.
%   """

%   The OPL solution is
%   """
%   Masonry  : 0..35
%   Carpentry: 35..50
%   Plumbing : 35..75
%   Ceiling  : 35..50
%   Roofing  : 50..55
%   Painting : 50..60
%   Windows  : 55..60
%   Facade   : 75..85
%   Garden   : 75..80
%   Moving   : 85..90
%   """

% The solution from minizinc solver:
% Note the adding of 1 compared to the OPL solution:
% (sligthly edited)
%   masonry:    1 - 35 --> 36
%   carpentry: 36 - 15 --> 51
%   plumbing:  36 - 40 --> 76
%   ceiling:   36 - 15 --> 51
%   roofing:   51 -  5 --> 56
%   painting:  51 - 10 --> 61
%   windows:   56 -  5 --> 61
%   facade:    76 - 10 --> 86
%   garden:    76 -  5 --> 81
%   moving:    86 -  5 --> 91
% 
% The solution from fdmip, Gecode/fz, Jacop/fz, ECLiPSe/ic, etc 
% that don't use output[] (slightly edited):
%    start = array1d(1..10, [ 1, 36, 36, 36, 51, 51, 56, 76, 76, 86]);
%    end   = array1d(1..10, [36, 51, 76, 51, 56, 61, 61, 86, 81, 91]);
%    limitx = 3;
%    makespan = 91;
%
% With the extra objective (change to `... minimize z`) the solution is
% (slightly edited)
%   masonry:   20 - 35 -->  55
%   carpentry: 75 - 15 -->  90
%   plumbing:  55 - 40 -->  95
%   ceiling:   75 - 15 -->  90
%   roofing:   90 -  5 -->  95
%   painting:  90 - 10 --> 100
%   windows:   95 -  5 --> 100
%   facade:    95 - 10 --> 105
%   garden:    95 -  5 --> 100
%   moving:   105 -  5 --> 110


% Compare with the Comet model
% http://www.hakank.org/comet/building_a_house.co
% 
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn"; 

% For output[]
array[1..10] of string: tasks = ["masonry","carpentry","plumbing","ceiling","roofing","painting","windows","facade","garden","moving"];
% for the precedences
int: masonry   = 1;
int: carpentry = 2;
int: plumbing  = 3;
int: ceiling   = 4;
int: roofing   = 5;
int: painting  = 6;
int: windows   = 7;
int: facade    = 8;
int: garden    = 9;
int: moving    = 10;

int: num_tasks = 10;
array[1..num_tasks] of int: duration =  [35,15,40,15, 5,10, 5,10, 5, 5];
array[1..num_tasks] of int: height   =  [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1];
int: total_duration = sum(t in 1..num_tasks) (duration[t]);

% precendeces
int: num_precedences = 14;
array[1..num_precedences, 1..2] of int: precedences;

% variables
array[1..num_tasks] of var 1..total_duration: start;
array[1..num_tasks] of var 1..total_duration: end;
var 1..3: limitx;
var 1..total_duration: makespan;

% the extra objective (cf the Comet model cited above)
var 0..10000: z = 400 * max(end[moving]- 100, 0)      +
                               200 * max(25 - start[masonry], 0)   +
                               300 * max(75 - start[carpentry], 0) +
                               100 * max(75 - start[ceiling], 0);
% var 0..10000: z = makespan;


% handle the precedences
predicate prec(int:x , int: y, array[int] of var int: s, array[int] of var int: d) =
   s[x] + d[x] <= s[y]
;

% solve minimize makespan;
solve :: int_search(
        start ++ [makespan, limitx], 
        % start ++ [makespan], 
        first_fail,
        indomain_min, 
        complete) 
    % satisfy;
    % minimize makespan;
    minimize z; % the extra objective


constraint
  % to be minimized
  maximum(makespan, end)/\
  cumulative(start, duration, height, limitx) /\

  forall(t in 1..num_tasks) (
     end[t] = start[t] + duration[t]
  )
  /\
 
  % precedences
  forall(p in 1..num_precedences) (
     prec(precedences[p,1], precedences[p,2], start, duration)
  )
;

precedences = array2d(1..num_precedences, 1..2, 
[
masonry,   carpentry, 
masonry,   plumbing, 
masonry,   ceiling,
carpentry, roofing,
ceiling,   painting,
roofing,   windows,
roofing,   facade,
plumbing,  facade,
roofing,   garden,
plumbing,  garden,
windows,   moving,
facade,    moving,
garden,    moving,
painting,  moving
]);

% Nice output, only with the minizinc solver
output [
  "makespan: " ++ show(makespan) ++ "\n" ++
  "z: " ++ show(z) ++ "\n"
] ++
[
  show(tasks[t]) ++ ": " ++ show(start[t]) ++ " - " ++ show(duration[t]) ++ " --> " ++ show(start[t] + duration[t]) ++ "\n"
  | t in 1..num_tasks
] ++ ["\n"];
