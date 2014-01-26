% 
% Kordemsky's Palindrome problem in MiniZinc.
% 
% From
% http://www.mathteacherctk.com/blog/2014/01/kordemskys-palindrome-problem/
% """
% Find a 10-digit number, with all digits distinct, whose quotient 
% of division by 9 is a palindrome, i.e., a number that is read the same from both ends. 
% """

% Note: I tested this on almost all the FlatZinc solvers I have access to.
%
%      * Some solvers, e.g. Gecode/fz and Chuffed just show the solutions 
%        where x and y starts with 1, probably some domain issue. 
%
%      * Some other solvers don't show any solutions at all, either
%        integer overflow or memory exhaustion:
%           JaCoP, Choco, fzn2smt, izplus, numberjack, 
%           mistral, picat, oscar, minisatid, eclipse/fd, G12/lazyFD
%
%      * Opturion CPX, minicsp, fzntini are very slow on this.
%
%      * These solvers show the 626 solutions:
%        - SICStus Prolog
%        - or-tools
%        - G12 fd, cpx
%        - eclipse/ic

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 10;
int: m = 9;

% decision variables
array[1..n] of var 0..9: xa;
%   1234567890  1234567890
var 1000000000..9999999999: x;

array[1..m] of var 0..9: ya;
%   123456789  123456789
var 10000000..999999999: y;

predicate to_num(array[int] of var int: a, var int: n) =
   let { int: len = length(a) } in
   n = sum(i in 1..len) ( pow(10, len-i) * a[i])
;


% solve satisfy;
solve :: int_search(xa ++ ya, first_fail, indomain_min, complete) satisfy;

constraint
  to_num(xa, x) 
  /\ alldifferent(xa)

  /\
  to_num(ya, y) 
  /\
  y = x div 9 
  /\
  forall(i in 1..m div 2) (ya[i] = ya[m-i+1])
;

output [
  "xa: ", show(xa), "\n",
  "x: ", show(x), "\n",
  "ya: ", show(ya), "\n",
  "y: ", show(y), "\n"
];

