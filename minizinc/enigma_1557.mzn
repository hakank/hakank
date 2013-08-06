% 
% Enigma 1557 (Reverse division) in MiniZinc.
% 
% Problem from F1 Compiler:
% http://www.f1compiler.com/samples/Enigma%201557.f1.html
% """
% Reverse divisiona
%
% Enigma 1557 Richard England, New Scientist magazine, August 8, 2009
% 
%  1. What is the four-digit integer divisible by 11 and 13 whose reverse
%     is a larger integer also divisible by 11 and 13?
% 
%  2. What is the four-digit integer divisible by 17 and 19 whose reverse 
%     is a larger integer also divisible by 17 and 19?
% 
%  Neither of your answers may have a leading zero.
% """ 
%
% This MiniZinc model is slighly more general than the F1 model, but 
% more complicated by using arrays.
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http:% www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 4; % n-digit numbers
int: max_num = ceil(pow(10.0,int2float(n)))-1;
var 10*n..max_num: n1;
var 10*n..max_num: n2;


% reverse an array
predicate reverse(array[int] of var int: x, array[int] of var int: rev) =
        let { int: len = card(index_set(x)) } in
        forall(i in index_set(x)) (
           rev[i] = x[len-i+1]
        )
;

%
% convert an array of digits <-> a number
%
predicate toNum10(array[int] of var int: a, var int: n) =
          let { int: len = length(a) } in
          n = sum(i in 1..len) (
            ceil(pow(10.0, int2float(len-i))) * a[i]
          )
          /\ forall(i in 1..len) (a[i] >= 0)
;


predicate enigma_1557(var int: x1, array[int] of int: divs, int: len) =
    let {
          var lb(x1)..ub(x1): x2,
          array[1..len] of var 0..9: a1,
          array[1..len] of var 0..9: a2
    } in
     forall(i in index_set(divs)) (
        x1 mod divs[i] = 0 /\
        x2 mod divs[i] = 0
     )
     /\
     x2 > x1 /\

     toNum10(a1, x1) /\
     reverse(a1, a2) /\
     toNum10(a2, x2) /\

     a1[1] > 0 /\ a2[1] > 0
 
;

% solve satisfy;
solve :: int_search([n1,n2], first_fail, indomain_min, complete) satisfy;

constraint
  enigma_1557(n1,[11,13],n)
  /\
  enigma_1557(n2,[17,19],n)
;

output
[
  "n1: " ++ show(n1) ++ "\n" ++
  "n2: " ++ show(n2) ++ "\n"
];
