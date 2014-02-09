% 
% Random generator in MiniZinc.
% 
% One of the thing I miss in MiniZinc is to be able
% to generate random problem instances. Here's one way...
%
% Inspiration: 
% http://en.wikipedia.org/wiki/Park%E2%80%93Miller_random_number_generator
% So, it's a simple RNG:
%    x[i+1] = g * x[i] mod n

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

int: n = 10;
int: max_value = 10;
int: seed; %  = 0;

% decision variables
array[1..n] of var int: rand; % random data

array[1..n] of var int: x;

% array[1..n] of var float: y;


%
% Returns (in r) an array of pseudo random elements with seed 'seed' and
% max_value 'max_value'. If max_value = 0, then no modulo is done in r.
%
% Example:
%    rand_int_array(10, 1, 10, r)
% give the array 
%    r = [1, 9, 4, 9, 3, 9, 2, 10, 6, 9] % seed 1
%
% Different seeds:
% seed = 1: [1, 9, 4, 9, 3, 9, 2, 10, 6, 9]
% seed = 2: [2, 7, 8, 7, 7, 7, 4, 9, 1, 8]
% seed = 3: [3, 5, 1, 6, 0, 5, 6, 8, 7, 7]
% seed = 4: [4, 3, 5, 4, 4, 4, 8, 7, 2, 5]
% seed = 1001: [0, 1, 8, 8, 10, 5, 2, 4, 10, 2]
%
predicate rand_int_array(int: n, int: seed, int: max_value, array[int] of var int: r) =
  let  {
    array[1..n] of var int: rng,
    %% int: rand_modn  = 2147483647; % 2^31 % too large
    %% int: rand_g = 16807; % 7^5
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

% don't work: "Error: Registry: Constraint float_div_reif not found in line no. 1106"
% predicate rand_float_array(int: n, int: seed, int: max_value, array[int] of var float: r) =
%   let  {
%     array[1..n] of var int: rng,
%   } in
%   rand_int_array(n, seed, max_value, rng)
%   /\
%   forall(i in 1..n) (
%     r[i] = int2float(max_value)/int2float(rng[i])
%   )
% ;


% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
   rand_int_array(n, seed, 10, rand) 
   % /\ rand_float_array(n, seed, max_value, y)

   /\ % just a silly test
   forall(i in 1..n-1) (
     x[i] >= 0 /\
     x[i+1]+x[i] = (rand[i]+rand[i+1])
   )

   
;

output [
  "rand: " ++ show(rand) ++ "\n" ++
  "x   : " ++ show(x) ++ "\n"
  
  % ++ "y  : " ++ show(y) ++ "\n"
];

