% 
% Numeric Keypad problem in MiniZinc.
% 
% From Fun with num3ers
% http://benvitale-funwithnum3ers.blogspot.com/2011/04/numeric-keypad.html
% """
% Rearrange the key caps of the 1 through 9 on a numeric keypad 
% so that no cap is on its correct key, in such a manner that 
% each of the three rows forms a 3-digit perfect square.
% 
%     7 8 9
%     4 5 6
%     1 2 3
% 
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 9;
int: m = 3;

% original configuration
array[1..m, 1..m] of 1..9: orig = array2d(1..m, 1..m, [
                                        7, 8, 9,
                                        4, 5, 6,
                                        1, 2, 3,
                                   ]);

array[1..m, 1..m] of var 1..9: x;


predicate toNum10(array[int] of var int: a, var int: n) =
          let { int: len = length(a) }
          in
          n = sum(i in 1..len) (
            ceil(pow(10.0, int2float(len-i))) * a[i]
          )
          /\ forall(i in 1..len) (a[i] >= 0)
;


% solve satisfy;
solve :: int_search([x[i,j] | i,j in 1..m], first_fail, indomain_min, complete) satisfy;

constraint
  alldifferent([x[i,j] | i,j in 1..m])
;

constraint
    % no fixpoint
    forall(i, j in 1..m) (
       x[i,j] != orig[i,j]
    )
;


constraint
    % each row makes a perfect square
    forall(i in 1..m) (
       let {
          var 100..999: t
       } in
       toNum10([x[i,j] | j in 1..m], t)
       /\
       exists(y in 10..32) (
          y*y = t
       )
    )
 
;

output [
   if j = 1 then "\n" else " " endif ++
      show(x[i,j])
   | i,j in 1..m
]
 ++ ["\n"]
;

