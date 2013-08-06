% 
% Consecutive digits puzzle in MiniZinc.
% 
% Martin Gardner (Kuly 1971)
% """
%   A B C D 
%   D C B A
% + . . . .
% ---------
% 1 2 3 0 0
% 
% ABCD are four consecutive digits in increasing order, DBCA are the same four
% in decreasing order. The four dots represent the same four digits in an
% unknown order. If the sum is 12300, what number is represented by the 
% four dots. (From W.T. Williams and G.H. Savage, 'The Strand Problems Book').
% """


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
var 1..9: A;
var 1..9: B;
var 1..9: C;
var 1..9: D;
array[1..4] of var 1..9: fd = [A,B,C,D];

array[1..4] of var 1..9: dots;
var int: dots_num;

var int: ABCD = 1000*A + 100*B + 10*C + D;
var int: DCBA = 1000*D + 100*C + 10*B + A;


predicate toNum10(array[int] of var int: a, var int: n) =
          let { int: len = length(a) }
          in
          n = sum(i in 1..len) (
            ceil(pow(10.0, int2float(len-i))) * a[i]
          )
          /\ forall(i in 1..len) (a[i] >= 0)
;

predicate contains(var int: e, array[int] of var int: a) =
   exists(i in 1..length(a)) (
      a[i] = e
   )
;

% solve satisfy;
solve :: int_search(fd ++ dots ++ [ABCD, DCBA, dots_num], first_fail, indomain_min, complete) satisfy;

constraint
  all_different(fd)
  /\
  increasing(fd)
  /\
  all_different(dots)
  /\
  toNum10(dots, dots_num)
  /\
  12300 = ABCD + DCBA + dots_num

  /\ % dots consist of the the digits A, B, C, and D
  contains(A, dots) 
  /\
  contains(B, dots) 
  /\
  contains(C, dots) 
  /\
  contains(D, dots) 
;

output [
   "  ", show(ABCD), "\n",
   "  ", show(DCBA), "\n",
   "+ ", show(dots_num), "\n",
   "------", "\n",
   " 12300","\n"

];
