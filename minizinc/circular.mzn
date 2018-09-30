% 
% Circular constraint in MiniZinc.
% 
% Ensure that all the numbers 1..n in array x of length m 
% are ordered and circular.
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 5; % domain
int: m = 10; % length of array

predicate circ(var int: a, var int: b, int: v) =
  b = if (1+a) mod v = 0 then v else 1+(a mod v) endif
;

%
% circular(x, max_value)
% Ensures that element x[i] = 1+x[i-1] (modulo max_value)
% i.e. circular ordered 
%
% Example: with a domain of 1..5 (max_value=5) there are 5 possible solutions:
% x: [1, 2, 3, 4, 5, 1, 2, 3, 5]
% x: [2, 3, 4, 5, 1, 2, 3, 4, 1]
% x: [3, 4, 5, 1, 2, 3, 4, 5, 2]
% x: [4, 5, 1, 2, 3, 4, 5, 1, 3]
% x: [5, 1, 2, 3, 4, 5, 1, 2, 4]
% 
predicate circular(array[int] of var int: a, int: v) =
  let {
    int: len = length(a)
  }
  in
  assert(len mod v = 0, "\(len) mod \(v) must be 0, is \(len mod v)!") /\
  forall(i in 2..len) (
    circ(x[i-1],x[i], v)
  )
  /\ circ(x[len],x[1], v) % around the corner
   
;

% decision variables
array[1..m] of var 0..n: x;

solve satisfy;

constraint
  circular(x, n)
;

output [
  "x: \(x)\n"
];

