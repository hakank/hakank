% 
% Random set generator in MiniZinc.
% 
% One of the thing I miss in MiniZinc is to be able
% to generate random problem instances. This model
% experiments with random sets of integers, both as
% a predicate (forced to be var ints) and fixed ints.
%
% Compare with the random integer (and float) generators:
%   http://www.hakank.org/minizinc/random_generator.mzn
%   http://www.hakank.org/minizinc/random_generator2.mzn
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

% include "globals.mzn"; 

int: n = 10;
int: m = 10; % "cardinality" of the set, either approximately or as a generator for the cardinality
int: max_value = 10;
int: seed; % = 2;

% decision variables
array[1..n] of var set of 0..max_value: rand; % random data

array[1..n] of var int: x;


% fixed variant
% note: m should be < max_value to be interesting
% the cardinality of the sets are (approximately) m
array[1..n] of set of int: rand_fixed = [ {(((seed*104729*j*i) mod 65537) mod max_value) | j in 1..m} | i in 1..n];

% more different cardinality
array[1..n] of set of int: rand_fixed2 = [ {(((seed*104729*j*i) mod 65537) mod max_value) | j in 1..1+(seed*(65537*i) mod m)} | i in 1..n];

array[1..n] of int: rand_fixed_lens = [ card(rand_fixed[i]) | i in 1..n];
array[1..n] of int: rand_fixed2_lens = [ card(rand_fixed2[i]) | i in 1..n];

%
% returns a random set 's' of size 'n' with 
% max value 'max_value' and seed 'seed'
% 
% This is based on the random generator in 
% http://www.hakank.org/minizinc/random_generator.mzn
%
predicate rand_int_set(int: n, int: seed, int: max_value, var set of int: s) =
  let  {
    array[1..n] of var int: rng,
    int: rand_modn = 65537,
    int: rand_g = 75,
    array[1..n] of var 0..max_value: r
  } in
  % generate the random sequence
  rng[1] = seed /\
  forall(i in 2..n) (
     rng[i] = (rand_g * rng[i-1]) mod rand_modn
  )
  /\ % restrict to 0..max_value
  forall(i in 1..n) (
     r[i]  = rng[i] mod (max_value+1)
  )
  /\ % "setify" it
  forall(v in 0..max_value+1) (
    v in s <-> exists(i in 1..n) (r[i] = v)
  )
;

%
% array of random sets
%
predicate random_set_matrix(int: n, 
                                int: m,
                                int: seed, 
                                int: max_value, 
                                array[int] of var set of int: s) =
    forall(i in 1..n) (
       % we must have different (deterministic) seeds
       rand_int_set(m, seed*i-1, max_value, s[i]) 
    )
;




solve satisfy;


constraint
   % forall(i in 1..n) (
   %    % note we must have different (deterministic) seeds
   %    rand_int_set(m, seed*i-1, max_value, rand[i]) 
   % )

   random_set_matrix(n,m,seed,max_value,rand)

   /\
   forall(i in 1..n) (
     % x[i] = sum(j in 1..max_value) (j*bool2int(j in rand[i]))
     x[i] = sum(j in 1..max_value) (j*bool2int(j in rand_fixed2[i]))
   )
   
;

output [
  "rand: " ++ show(rand) ++ "\n" ++ 
  "rand_lens: " ++ show([card(fix(rand[i])) | i in 1..n]) ++ "\n\n" ++
  "x: " ++ show(x) ++ "\n" ++
  "rand_fixed: " ++ show(rand_fixed) ++ "\n" ++
  "rand_fixed_lens: " ++ show(rand_fixed_lens) ++ "\n" ++
  "rand_fixed2: " ++ show(rand_fixed2) ++ "\n" ++ "\n" ++
  "rand_fixed2_lens: " ++ show(rand_fixed2_lens) 

]
++
[
  show(rand_fixed2[i]) ++ "\n"
  | i in 1..n
]
;

