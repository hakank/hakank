% 
% Reverse multiplies in MiniZinc.
% 
% Find reverse multiples.
% 
% E.g.
%  4*2178 = 8712
%
%
% Also see:
%   - Richard Green: "Reverse multiple numbers"
%     https://plus.google.com/u/0/101584889282878921052/posts/9HFfd99sFj2
%
%   - N.J.A. Sloane: "2178 And All That"
%     http://arxiv.org/abs/1307.0453
% via
% 
% Jekejeke Logic Programming:
% https://plus.google.com/u/0/103259555581227445618/posts/9HE8iXQaXnM
% 

%
% Here are the found solutions for base 10 and n from 2..9
% 
% [n, base, num1, num2, m]
% [4, 10, 9801, 1089, 9]
% [4, 10, 8712, 2178, 4]
% [5, 10, 98901, 10989, 9]
% [5, 10, 87912, 21978, 4]
% [6, 10, 989901, 109989, 9]
% [6, 10, 879912, 219978, 4]
% [7, 10, 9899901, 1099989, 9]
% [7, 10, 8799912, 2199978, 4]
% [8, 10, 98019801, 10891089, 9]
% [8, 10, 98999901, 10999989, 9]
% [8, 10, 87128712, 21782178, 4]
% [8, 10, 87999912, 21999978, 4]
% [9, 10, 980109801, 108901089, 9]
% [9, 10, 989999901, 109999989, 9]
% [9, 10, 871208712, 217802178, 4]
% [9, 10, 879999912, 219999978, 4]
%
% Some base 9 solutions
%
% base: 9
% num1: 5248
% num2: 1312
% a1  : [7, 1, 7, 1]
% a2  : [1, 7, 1, 7]
% m  : 4
% ----------
% base: 9
% num1: 5824
% num2: 1456
% a1  : [7, 8, 8, 1]
% a2  : [1, 8, 8, 7]
% m  : 4
% ----------
% base: 9
% num1: 6400
% num2: 800
% a1  : [8, 7, 0, 1]
% a2  : [1, 0, 7, 8]
% m  : 8
% ----------
% base: 9
% num1: 4800
% num2: 2400
% a1  : [6, 5, 2, 3]
% a2  : [3, 2, 5, 6]
% m  : 2


% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 4;
int: base = 10;

int: lower = pow(base,n-1);
int: upper = pow(base,n)-1;
var lower..upper: num1;
var lower..upper: num2;

var 2..base: m; % the multiplier

array[1..n] of var 0..base-1: a1;
array[1..n] of var 0..base-1: a2;

predicate to_num(array[int] of var int: a, var int: n, int: b) =
   let { int: len = length(a) } in
   n = sum(i in 1..len) (
      pow(b, len-i) * a[i]
   )
;

% a is b reversed
predicate reverse(array[int] of var int: a,
                  array[int] of var int: b) =
    let { int: len = length(a) } in
    forall(i in 1..len) (
        a[i] = b[len-i+1]
    )
;

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint

   % num1 = 2178 /\
   num2 * m = num1 /\
   to_num(a1,num1, base) /\
   to_num(a2,num2, base) /\   
   reverse(a1,a2)
;

output [
  % show([n,base,num1,num2,m]) ++ "\n" % ++
  "base: " ++ show(base) ++ "\n" ++
  "num1: " ++ show(num1) ++ "\n" ++
  "num2: " ++ show(num2) ++ "\n" ++
  "a1  : " ++ show(a1) ++ "\n" ++
  "a2  : " ++ show(a2) ++ "\n" ++
  "m  : " ++ show(m) ++ "\n"

];

