%
% Hamming distance in Minizinc
%
% I.e. the number of bits differing in two (binary) arrays.
% See http://en.wikipedia.org/wiki/Hamming_distance
% 
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: n = 6; % length of the arrays
array[1..n] of var 0..1: a;
array[1..n] of var 0..1: b;
var int: diffs; % The number of differences 

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


solve satisfy;

%
% We can now either
% - Calculate the hamming distance from two arrays
% - Given the distance, generate all arrays which has the hamming distance
%
predicate hamming_distance(array[int] of var int: a, array[int] of var int: b, var int: d) =
     d = sum(i in 1..length(a)) (bool2int( a[i] != b[i]))   
;

constraint
     cp1d(a, [1,1,1,1,1,1]) /\
     hamming_distance(a, b, diffs)
     /\ 
     diffs = 2 
;


output [
  "a: " ++ show(a) ++ "\n" ++
  "b: " ++ show(b) ++ "\n" ++
  "diffs: " ++ show(diffs) ++ "\n"
];
