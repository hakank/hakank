% 
% Enigma puzzle Eight times (#1317) in MiniZinc.
% 
% http://www.f1compiler.com/samples/Enigma%201317.f1.html
% """
%  Eight times
%
%  Enigma 1317 Richard England, New Scientist magazine
%
%  I invite you to find a 5-digit number consisting of five different digits 
%  (not starting with zero) which when multiplied by 8 produces another 5-digit 
%  number consisting of the other 5-digits; in addition the sum of the digits of 
%  your 5-digit number must be greater than the sum of the digits of the product.
%  
%  Which 5-digit number have you found? (Remember that the answer required is 
%  the number as it was before the multiplication.)
%
% """
% 

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% This is a different model compared to enigma_eight_times.mzn

include "globals.mzn"; 

int: n = 5;
var 10000..99999: x;
var 10000..99999: y;
array[1..n] of var 0..9: x_a;
array[1..n] of var 0..9: y_a;

predicate toNum10(array[int] of var int: tal, var int: summa) =
    let { int: len = length(tal) }
    in
    summa = sum(i in 1..len) (
       ceil(pow(10.0, int2float(len-i))) * tal[i]
    )
    /\ forall(i in 1..len) (tal[i] >= 0)
;

solve :: int_search(x_a ++ y_a, first_fail, indomain_min, complete) satisfy;

constraint
  all_different(x_a ++ y_a) 
  /\
  toNum10(x_a, x)
  /\
  toNum10(y_a, y)
  /\
  y = 8 * x
  /\
  sum(x_a) > sum(y_a)
;


output
[
 "x: " ++ show(x) ++ "\n" ++
 "y: " ++ show(y) ++ "\n" ++
 "x_a: " ++ show(x_a) ++ "\n" ++
 "y_a: " ++ show(y_a) ++ "\n"
];
