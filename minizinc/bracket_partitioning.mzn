% 
% Bracket partitioning in MiniZinc.
% 
% From 
% "Set partitioning with constraints java":
% http://stackoverflow.com/questions/21489938/set-partitioning-with-constraints-java
% """
% The what
%
% I'm attempting to produce an optimum set of brackets (optimum defined by 
% constraints) for a tournament.
% 
% The problem
% 
% I don't know how to approach the problem. This paper is pretty high level 
% but discusses the possibility of solving set partitioning problems with constraint 
% programming. It also states that the majority of set partitioning problems 
% are solved via integer programming. I am mainly looking for an example to emulate. 
% The question is similar to this SO question. The majority of the constraint 
% examples I've seen defined a specific partition total. Is it possible to model 
% a system where the partitions would be determined dynamically by the constraints 
% and the participant set? I would link examples but I am limited to only 2 due 
% to my reputation.
% 
% A more concrete example
% 
% Known values
% 
%  - The number of participants is N.
%  - Each participant has a weight W associated with them.
%
% Constraints
%
%  - A bracket (set) is made up of 2,3,4,6,7, or 8 participants.
%  - Each participant is only in a single bracket.
%  - The must not be more than a 15% difference between the lowest 
%    weighted participant and the highest weighted participant in a bracket.
%  -   Favor creating brackets of size 8 and 4 over all other bracket sizes.
% 
% So for example say there are 8 participants.
% 
% { {1, W=100}, {2, W=103}, {3, W=105}, {4, W=106}, {5, W=110}, {6, W=114}, {7, W=120}, {8, W=125} }
% 
% One possible solution would be: {1, 2, 3}, {4, 5}, {6, 7, 8}
% 
% A more optimum solution would be: {1, 2, 3, 4}, {5, 6, 7, 8} since this 
% favors 4, 8 sized sets over the previous solution.
% 
% Is it possible to partition a set into a dynamic number of 
% child sets?
% 
% Thanks again for your time!
% """

% Note: A natural approach would be to use set variables but
% since quite a few solvers don't support that, I start with
% a pure FD approach.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

% int: n = 8; % original exampel
% array[1..n] of int: w = [100,103,105,106,110,114,120,125];  % original example

% int: n = 18; 
% array[1..n] of int: w = [111, 109, 112, 146, 115, 103, 130, 145, 128, 127, 144, 114, 133, 126, 134, 133, 114, 134]; % for n=18 (seed was 11)

% int: n = 28; 
% array[1..n] of int: w = [111, 109, 112, 146, 115, 103, 130, 145, 128, 127, 144, 114, 133, 126, 134, 133, 114, 134, 143, 116, 106, 104, 147, 110, 114, 102, 118, 130]; % for n=28 (seed was 11)
% sorted variant
% array[1..n] of int: w = [102,103,104,106,109,110,111,112,114,114,114,115,116,118,126,127,128,130,130,133,133,134,134,143,144,145,146,147];

int: n = 30;

% random weights (calculated below by rand_int_array)
% array[1..n] of var int: w;
array[1..n] of int: w = [ 100+(((seed* 104729*i) mod 65537) mod 100) | i in 1..n];


set of int: set_sizes0 =  {0,2,3,4,6,7,8}; % valid set sizes + 0
% set of int: set_sizes =  {2,3,4,6,7,8};

% decision variables
array[1..n] of var 1..n: x; % which set does a person belong to.

% size of each set
int: ssize = n div 2; % max number of brackets
int: smax = 200; % this should be max(w) but since I sometimes have random weights, it don't work
array[1..ssize] of var set_sizes0: set_size;
array[1..ssize] of var 0..smax: mins;
array[1..ssize] of var 0..smax: maxs;
array[1..ssize] of var 0..smax: diffs; % difference maxs[i]-mins[i]

% prefer group sizes of 4 and 8
var 0..n: z = sum([bool2int(set_size[s] in {4,8}) | s in 1..ssize ]);
% here we (also) prefer 8 (weight 2) over 4 (weight 1):
% var 0..2*n: z = sum([ 1*bool2int(set_size[s] = 4) | s in 1..8 ]) + sum([ 2*bool2int(set_size[s] = 8) | s in 1..8 ]); 

solve :: int_search(x ++ set_size, max_regret, indomain_split, complete) maximize z;
% solve :: int_search(x, first_fail, indomain_split, complete) minimize sum(diffs);



%
% Randomize the weights (just for tests)
%
% predicate rand_int_array(int: n, int: seed, int: min_value, int: mod_value, array[int] of var int: r) =
%   let  {
%     int: rand_modn = 65537,
%     array[1..n] of var 0..rand_modn: rng,
%     %% int: rand_modn  = 2147483647; % 2^31 % too large
%     %% int: rand_g = 16807; % 7^5
%     int: rand_g = 75
%   } in
%   rng[1] = seed /\
%   forall(i in 2..n) (
%      rng[i] = (rand_g * rng[i-1]) mod rand_modn
%   )
%   /\
%   forall(i in 1..n) (
%     if  mod_value > 0 then
%        r[i] = (min_value + (rng[i] mod (mod_value+1)))
%     else 
%        r[i] = rng[i]
%     endif
%   )
% ;

%
% get the minimum value of x where x[i] > 0
%
predicate min_except_0(var int: min_val, array[int] of var int: x) =
    exists(i in index_set(x)) (
      min_val = x[i] /\ 
      forall(j in index_set(x) where i != j) (
        min_val <= x[j]
        \/ % ignore 0
        x[j] = 0
      )
    )
    /\
    min_val > 0
;


% randomize
int: seed = 11;
% constraint rand_int_array(n, seed, 100, 50, w);

%
% who belongs in the same bracket:
% """
% There must not be more than a 15% difference between the lowest 
% weighted participant and the highest weighted participant in a bracket.
% """
%
constraint
  % connect x and set_size
  global_cardinality(x, [i | i in 1..ssize], set_size) 
  /\ 
  forall(s in 1..ssize) (
         ( % for non empty brackets we calculate differences etc
           set_size[s] > 0 ->
          (
            min_except_0(mins[s],[w[i]*bool2int(x[i]=s) | i in 1..n]) /\
            maxs[s] = max([w[i]*bool2int(x[i]=s) | i in 1..n]) /\
            diffs[s] = maxs[s] - mins[s]
            % 15% differences between the lowest and highest
            % (Note: Most MiniZinc solver just handle integer division, hence the hack.)
            /\ 10000*diffs[s] div maxs[s] <= 1500
           )
         )
        /\ % handle empty bracket sizes 
        (
         set_size[s] = 0 ->
             (diffs[s] = 0 /\ mins[s] = 0 /\ maxs[s] = 0)
        )
  )
;

% symmetry breaking and implicit constraints
constraint
  sum(set_size) = n % implicit constraint

  %% the brackets should be numbered from 1..
  % /\ forall(i in 2..ssize) (
  %    exists(j in 1..n) ( x[j] = i  ) -> exists(j in 1..n) ( x[j] = i-1  )
  % ) 
  /\
  value_precede_chain([ i | i in 1..ssize], x) % global constraint

  % symmetry breaking
  /\ x[1] = 1 % symmetry breaking: place first person in bracket 1
  % /\ (x[2] = 1 \/ x[2] = 2)
  /\ decreasing(set_size)
;

% bracket sizes
constraint
   forall(s in 1..ssize) (
      set_size[s] = sum(j in 1..n) ( bool2int(x[j]=s))
   )
;

output [
  "w: " ++ show(w) ++ "\n" ++
  "z: " ++ show(z) ++ "\n" ++
  "x: " ++ show(x) ++ "\n" ++
  "set_size: " ++ show(set_size) ++ "\n" ++ 
  "diffs: " ++ show(diffs) ++ "\n" ++
  "mins: " ++ show(mins) ++ "\n" ++
  "maxs: " ++ show(maxs) ++ "\n" 
]
++
[
  if fix(set_size[s]) > 0 then 
     "bracket " ++ show(s) ++ ": " ++ show([w[i] | i in 1..n where fix(x[i]) = s]) ++ "\n"
  else
    ""
  endif
  % | s in 1..n
  | s in 1..ssize
]
;

