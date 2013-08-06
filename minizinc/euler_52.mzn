% 
% Euler problem 52 in MiniZinc.
% 
% """
% It can be seen that the number, 125874, and its double, 251748,
% contain exactly the same digits, but in a different order.
% 
% Find the smallest positive integer, x, such that 2x, 3x, 4x, 5x,
% and 6x, contain the same digits.
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% Asumption: base 10
% Answer: x = 142857
% 

int: n = 6;
int: d = 9;
int: base = 10; % generalized to any base
array[1..n, 1..d] of var 0..base-1: digits;
array[1..n] of var int: x;


% Base = 10
predicate toNum10(array[int] of var int: a, var int: n) =
          let { int: len = length(a) }
          in
          n = sum(i in 1..len) (
            ceil(pow(10.0, int2float(len-i))) * a[i]
          )
          /\ forall(i in 1..len) (a[i] >= 0)
;


predicate toNum(array[int] of var int: a, var int: n, float: base) =
          let { int: len = length(a) }
          in
          n = sum(i in 1..len) (
            ceil(pow(base, int2float(len-i))) * a[i]
          )
          /\ forall(i in 1..len) (a[i] >= 0)
;


%
% does a contains e?
%
predicate contains(var int: e, array[int] of var int: a) =
   exists(i in 1..length(a)) (
      a[i] = e
   )
;


% solve satisfy;
solve :: int_search(
        x ++ [digits[i,j] | i in 1..n, j in 1..d], 
        first_fail, 
        indomain_split, 
        complete) 
        % satisfy;
        minimize x[1];

constraint

  x[1] > 0    
  /\
  forall(i in 1..n) (
    x[i] >= 0
    /\
    x[i] = i * x[1]
    /\
    toNum([digits[i, j] | j in 1..d], x[i], int2float(base))
  )
  /\
 forall(i in 2..n) (
    % all digits must be the same (in some order)
    forall(j in 1..d) (
        contains(digits[i,j], [digits[i-1, k] | k in 1..d] )
        /\
        contains(digits[i-1,j], [digits[i, k] | k in 1..d] )
    )
 )
;

output [
  "\nx: ", show(x), 
] ++ 
[
  if j = 1 then "\n" else " " endif ++
    show(digits[i,j])
  | i in 1..6, j in 1..9
];