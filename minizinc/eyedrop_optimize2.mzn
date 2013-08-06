% 
% Optimize eye drops in MiniZinc.
% 
% After a Cataract operation that didn't go well - they 
% didn't put back the lens - I am ordinated three different 
% eye drops to take each day (for a couple of some weeks from now):
%
%   6 drops per day of kind 1 (later: 4 drops)
%   3 drops per day of kind 2
%   1 drop per day of kind 3 (for the night)
%
% What is the (an) optimal way of distributing these drops, provided:
%
%  - the day start at 7:00 and end at 23:00
%  - the different types should be distributed as "even as possible"
%    (whatever that mean)
%  - there may just be one drop per hour
%  - drop 3 is taken last ("for the night")
%
% The objective is then to separate the times to take an eye drop type 
% as much as possible (variables z is the sum of dispersion).
%
%
% Here is one solution to the original ([6,3,1]) problem:
%
%   z:   : 36
%   x    : [1, 2, 0, 1, 0, 0, 1, 2, 0, 1, 0, 0, 1, 2, 0, 1, 3]
%   dists: [30, 6, 0]
%    7: 1
%    8: 2
%    9:   
%   10: 1
%   11:   
%   12:   
%   13: 1
%   14: 2
%   15:   
%   16: 1
%   17:   
%   18:   
%   19: 1
%   20: 2
%   21:   
%   22: 1
%   23: 3
% 
%   Type 1: 7 10 13 16 19 22 
%   Type 2: 8 14 20 
%   Type 3: 23 
% 
% Another solution also with z = 36 (optimal):
%
%   Type 1: 7 10 13 16 19 22 
%   Type 2: 9 15 21 
%   Type 3: 23 

%
%
% Here is one solution to the [4,3,1] problem:
% 
%   z:   : 21
%   x    : [1, 2, 0, 0, 0, 1, 0, 2, 0, 0, 1, 0, 0, 2, 0, 1, 3]
%   dists: [15, 6, 0]
%   ...
%   Type 1: 7 12 17 22 
%   Type 2: 8 14 20 
%   Type 3: 23 
%
% 
%
%
% For more about Cataract surgery:
% https://en.wikipedia.org/wiki/Cataract_surgery

% Note: For some solvers, this is a much faster model than
%   http://www.hakank.org/minizinc/eyedrop_optimize.mzn
% mostly because there are no temporary variables (they have been
% replaced by drop_table and dists_table which are used in the labeling.
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

% The range of time to consider (per day)
int: start; % = 7;
int: end; % = 23;
int: hours = end - start+1;

% Number of different drops
int: num_types;

% Number of drops to take per day (per type)
array[1..num_types] of int: occ;

int: max_times = max(occ);

% decision variables

 % The time table. 0 means that there is no drop to be taken this hour.
array[start..end] of var 0..num_types: x;
% dispersion per drop type
var 0..1000: z;

array[1..num_types, 1..max_times] of var start..end: drop_table;
array[1..num_types, 1..max_times-1] of var 0..hours*num_types: dists_table;

%
% This predicate calculate the times a certain drop type should be taken.
% "dist" is the dispersion factor for these drops.
% It use two temporary variables:
%   Y: The times to take this drop type
%   YDist: The differences between the times in Y 
%          (used for the dispersion factor) 
%
predicate make_drop(array[int] of var int: a, int: n, int: t, array[int] of var int: Y, array[int] of var int: YDist, int: start, int: end, int: hours) =
   alldifferent(Y) 
   /\
   increasing(Y)

   /\ 
   forall(i in 1..n-1) ( YDist[i] = Y[i+1]-Y[i])

   % Connect the time table (a) with the times of this drop
   /\ forall(i in start..end) ( a[i] = t <-> exists(j in 1..n) ( Y[j] = i) )
   %% another approach
   % /\ forall(j in 1..length(Y)) ( exists(i in start..end) (Y[j] = i /\ a[i] = t) )

   %% If we want to put some weight on the different type of drops for larger 
   %% dispersion, we can multiply the sum with - say - t (the type).
   % dist = t*sum(i,j in 1..n-1 where i < j) (YDist[j] * bool2int(YDist[j] = YDist[i])) 


;

% solve maximize z;
% solve satisfy;
solve :: int_search(
        [drop_table[t,j] | t in 1..num_types, j in 1..max_times] ++ 
        [dists_table[t,j] | t in 1..num_types, j in 1..max_times-1] 
        ,
        most_constrained, 
        indomain_min,
        complete) 
    maximize z;
    % satisfy;

constraint
  assert(sum(occ) <= hours, "Total number of drops [sum(occ)] must be <= hours.")
;

constraint

   global_cardinality(x, [i | i in 1..num_types], occ) :: domain
   /\
   forall(t in 1..num_types) (
      make_drop(x, occ[t], t, 
                [drop_table[t,j] | j in 1..occ[t]], [dists_table[t, j] | j in 1..occ[t]-1], 
                start, end, hours)

      /\ % fix the unknowns (after occ[t]) in drop_table and dists_table since
         % they are not used in make_drop
      forall(j in occ[t]+1..max_times) (
        drop_table[t,j] = start
      )
      /\
      forall(j in occ[t]..max_times-1) (
        dists_table[t,j] = 0
      )

   )


   /\ % Heuristic to maximize the dispersion
   z = sum(t in 1..num_types, i,j in 1..occ[t]-1 where i < j) (dists_table[t,j] * bool2int(dists_table[t,j] = dists_table[t,i])) 



   % Specific constraint: 
   % Type 3 is the last drop to take (not necessarily at the last hour)
   % Note: It assume a single occurrence of the drop type.
   /\ 
   if occ[num_types] = 1 then
     forall(i in start..end) (
         x[i] = num_types -> forall(j in i+1..end) (x[j] = 0)
     )
   else 
     true
   endif

   % /\ % symmetry breaking
   % value_precede_int(1, 2, x) /\
   % value_precede_int(1, 3, x) /\
   % value_precede_int(2, 3, x)

   % Test of original problem (for generating all solutions with optimal z)
   % /\ z = 36
   % /\ z = 21 % Test of the later ordination

;


%
% Data
%

% Original problem
start = 7;
end = 23;
num_types = 3;
occ = [6,3,1];

% This is a later ordination:
% start = 7;
% end = 23;
% num_types = 3;
% occ = [4,3,1];

% Test
%start = 7;
%end = 23;
%num_types = 5;
%occ = [6,5,3,2,1];

% Test 2
% start = 1;
% end = 30;
% num_types = 6;
% occ = [8,5,4,3,2,1];


output [
  "z:   : " ++ show(z) ++ "\n" ++
  "x    : " ++ show(x) ++ "\n" % ++
]
++
[
  show_int(2,t) ++ ": " ++ 
     if fix(x[t]) > 0 then
        show(x[t])
     else 
        "  "
     endif
     ++ "\n"
  | t in start..end
]
++
[
  if time = start then "\nType " ++ show(t) ++ ": " else "" endif ++
  if fix(x[time]) == t then 
    show_int(2, time) ++ " "
  else 
    ""
  endif

  | t in 1..num_types, time in start..end
]
++ ["\ndrop_table:"] ++
[
  if time = 1 then"\n" else " " endif ++
   show_int(2, drop_table[t,time])
 
  | t in 1..num_types, time in 1..occ[t]
]
++ ["\ndists_table:"] ++
[
  if time = 1 then"\n" else " " endif ++
   show_int(2, dists_table[t,time])
 
  | t in 1..num_types, time in 1..max_times-1
]
++
[ "\nz: " ++ show(z) ++ "\n"]
;

