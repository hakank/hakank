% 
% Function partitions in MiniZinc.
%
% Partitions the integers 1..n according to a function, e.g.
%  - mod
%  - div 
%  - a boolean test, e.g. i > 3
%
% The length of the arrays might be overkill, especially for the boolean
% tests...
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: n;
array[0..n] of var set of 1..n: x;
% the sums of the partitions
array[0..n] of var int: sums;
% binary matrix of which integer is included in which partition
array[0..n, 0..n] of var 0..1: x_bool;

predicate set_sum(var set of int: s, var int: the_sum) =
   the_sum = sum(i in ub(s)) (bool2int(i in s)*i)
;


solve :: set_search(x, input_order, indomain_min, complete) satisfy;
% solve satisfy;

constraint
   forall(i in 1..n) (
       % hmm, only minizinc can handle this simple one:
       % i in x[n `mod` i] 
       %
       % instead we must also explicitly state that i is not in any 
       % other set than x[n mod i].
       let {
           % int: z = n `mod` i
           int: z = n mod i
           % int: z = n `div` i
           % int: z = bool2int( i > 3)
       }
       in
       i in x[z]
       % ah, this can - of course - be replaced with the global constraint
       % partition_set
       % /\
       % forall(j in 0..n where j != z) (
       %    not (i in x[j])
       % )
   )
   /\ % make sure that a value is only in one partition
      % Note: eclipse don't like this: dvar_remove_smaller
   partition_set(x, 1..n)

   /\ % the sums
   forall(i in 0..n) (
      set_sum(x[i], sums[i])
   )

   /\ % the binary (0..1) matrix
   forall(i in 0..n) (
     link_set_to_booleans(x[i], [x_bool[i,j] = 1 | j in 0..n])
   )
;
       
output [
  "\nx: ", show(x),"\n",
  "set_sums: ", show(sums),"\n"
]  ++
[
  if j = 0 then "\n" else " " endif ++
  show(x_bool[i,j])
  | i,j in 0..n
] ++ ["\n"];


%
% data
%
n = 10;