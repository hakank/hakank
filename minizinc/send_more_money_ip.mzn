% 
% Send More Money - integer programming version in MiniZinc.
% 
% This is an integer programming version of the SEND+MORE=MONEY problem.
% It was "inspired" by (i.e. remodelled from) the GLPK model money.mod.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 

set of int: DIGITS = 0..9;

% Constants for positon in dig
int: D = 1;
int: E = 2;
int: M = 3;
int: N = 4;
int: O = 5;
int: R = 6;
int: S = 7;
int: Y = 8;

int: n = 8;
array[1..n] of var DIGITS: LETTERS = [D,E,M,N,O,R,S,Y];

%  x[i,d] = 1 means that letter i is digit d
array[1..n, DIGITS] of var 0..1: x;

% carry bits
array[1..3] of var 0..1: carry;

% dig[i] is a digit corresponding to letter i
array[1..n] of var DIGITS: dig;


solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint

  % each letter must correspond exactly to one digit
  forall(i in 1..n) (
        sum(d in DIGITS) (x[i,d]) = 1
  )
  /\
  % different letters must correspond to different digits; note that
  % some digits may not correspond to any letters at all
  forall(d in DIGITS) (
        sum(i in 1..n) (x[i,d]) <= 1
  )
  /\
  forall(i in 1..n) (
        dig[i] = sum(d in DIGITS) (d * x[i,d])
  )
  /\

  dig[D] + dig[E]            = dig[Y] + 10 * carry[1] /\
  dig[N] + dig[R] + carry[1] = dig[E] + 10 * carry[2] /\ 
  dig[E] + dig[O] + carry[2] = dig[N] + 10 * carry[3] /\ 
  dig[S] + dig[M] + carry[3] = dig[O] + 10 * dig[M]   /\ 
  dig[M] >= 1   % M must not be 0

;

output [
  "digits: ", show(dig), "\n"

];
