% 
% Contiguity constraint (MIP) in MiniZinc.
% 
% From 
% https://www.or-exchange.org/questions/9203/forcing-the-value-of-consecutive-variables
% 
% This model use fbahr's answer.
% This is int version using 0/1 as weights.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 100;
set of int: T = 1..n;

int: A = 10;
int: seed = 8;

% random 0/1
% array[1..n] of int: w = [0,0,1,0,0,0,0,0,1,0,1,1,1,0,1,1,0,1,1,0,0,1,0,0,1,1,1,1,0,1,0,1,0,1,0,1,1,1,0,1,0,0,0,0,1,0,1,1,0,0,0,1,0,0,1,1,0,0,0,1,1,0,1,1,1,1,1,0,0,1,1,1,0,1,1,0,1,0,1,1,1,1,0,0,1,1,0,1,0,1,0,0,0,1,0,0,0,1,1,1];
% array[1..n] of int: w = [0,0,0,1,0,1,0,1,1,0,0,0,1,1,1,1,1,0,0,1,1,0,0,1,1,0,1,1,0,1,1,0,0,0,1,1,1,0,0,0,1,1,1,0,0,0,0,1,0,1,1,1,0,0,0,1,1,1,1,1,1,1,0,1,1,0,1,0,0,1,1,1,1,0,1,1,0,0,0,1,0,1,0,0,1,0,1,1,1,0,0,0,0,1,1,0,0,1,1,0];

array[1..n] of var 0..1: w; % random values (given seed)


predicate rand_int_array(int: n, int: seed, int: max_value, array[int] of var int: r) =
  let  {
    array[1..n] of var int: rng,
    int: rand_modn = 65537,
    int: rand_g = 75
  } in
  rng[1] = seed /\
  forall(i in 2..n) (
     rng[i] = (rand_g * rng[i-1]) mod rand_modn
  )
  /\
  forall(i in 1..n) (
    if  max_value > 0 then
       r[i] = rng[i] mod (max_value+1)
    else 
       r[i] = rng[i]
    endif
  )
;



% decision variables
array[T] of var 0..1: y;
var 0..n: z;

% int version
predicate scalar_product(array[int] of var int: x, array[int] of var int: v, var int: s) =
   s = sum(i in index_set(x)) (x[i]*v[i])
;

% float version
predicate scalar_product(array[int] of var int: x, array[int] of float: v, var float: s) =
  s = sum(i in index_set(x)) (int2float(x[i])*v[i])
;

solve minimize z;
% solve satisfy;
% solve :: int_search(y, first_fail, indomain_min, complete) minimize z;

constraint
  rand_int_array(n,seed,1,w) /\

  scalar_product(y,w,z) 
  /\
  z >= A


  /\ % consecutive_ones 
  forall(i in 1..n-1) (
     n * (1 - y[i] + y[i+1]) >= sum(t in T where t >= i+1) (y[t])
  )
;

output [
  "z: ", show(z), "\n",
  "w: ", show(w), "\n",
  "y: ", show(y), "\n"
];

