% 
% Enigma problem 843 (How many are whole?) in MiniZinc.
% 
% Problem formulation from 
% http://magma.maths.usyd.edu.au/magma/Examples/node4.html
% """
% The problem `How many are whole?', by Susan Denham, was taken from 
% New Scientist No. 1998 (7 Oct 1995), in the Enigma problem section 
% on p. 63. 
%
% [...]
%
% The Problem:
% 
% In what follows, digits have been consistently replaced by letters, 
% with different letters being used for different digits:
% 
% In the list ONE TWO THREE FOUR just the first and one other are perfect squares.
% In the list ONE+1 TWO+1 THREE+1 FOUR+1 just one is a perfect square.
% In the list ONE+2 TWO+2 THREE+2 FOUR+2 just one is a perfect square.
% 
% If you want to win the prize send in your FORTUNE. 
% 
% ...
%
% The answer published a few weeks later was FORTUNE = 3701284. 
%
% """
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%
include "globals.mzn"; 

int: n = 9;
set of int: Digits = 0..9;

var Digits: O;
var Digits: N;
var Digits: E;
var Digits: T;
var Digits: W;
var Digits: H;
var Digits: R;
var Digits: F;
var Digits: U;

array[1..n] of var Digits: x = [O,N,E,T,W,H,R,F,U];

var 100..999:     ONE  ;
var 100..999:     TWO  ;
var 10000..99999: THREE;
var 1000..9999:   FOUR ;
var 1000000..9999999: FORTUNE;

predicate square(var int: y) =
  exists(z in 1..ceil(sqrt(int2float(ub(y))))) (
        z*z = y
  )
;

predicate toNum(array[int] of var 0..9: a, var int: n) =
          let { int: len = length(a) } in
          n = sum(i in 1..len) (
            ceil(pow(10.0, int2float(len-i))) * a[i]
          )
;

predicate cp1d(array[int] of var bool: x, array[int] of var bool: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


% solve satisfy;
% solve :: int_search(x ++ [ONE, TWO, THREE, FOUR, FORTUNE], anti_first_fail, indomain_min, complete) satisfy;
solve :: int_search(x ++ [ONE, TWO, THREE, FOUR, FORTUNE], first_fail, indomain_min, complete) satisfy;

constraint
  all_different(x) /\

  toNum([O,N,E], ONE)             /\
  toNum([T,W,O], TWO)             /\
  toNum([T,H,R,E,E], THREE)       /\
  toNum([F,O,U,R], FOUR)          /\
  toNum([F,O,R,T,U,N,E], FORTUNE) /\

  % In the list ONE TWO THREE FOUR just the first and one other are perfect squares.
  % square(ONE) /\ ( square(TWO) \/ square(THREE) \/ square(FOUR)) /\
  square(ONE) /\ ( bool2int(square(TWO)) + bool2int(square(THREE)) + bool2int(square(FOUR))) = 1 /\

  % % In the list ONE+1 TWO+1 THREE+1 FOUR+1 just one is a perfect square.   
  (bool2int(square(ONE+1)) + bool2int(square(TWO+1)) + bool2int(square(THREE+1)) + bool2int(square(FOUR+1))) = 1 /\

  % In the list ONE+2 TWO+2 THREE+2 FOUR+2 just one is a perfect square.
  (bool2int(square(ONE+2)) + bool2int(square(TWO+2)) + bool2int(square(THREE+2)) + bool2int(square(FOUR+2))) = 1

;


output [
  "ONE: " ++ show(ONE) ++ "\n" ++
  "TWO: " ++ show(TWO) ++ "\n" ++
  "THREE: " ++ show(THREE) ++ "\n" ++
  "FOUR: " ++ show(FOUR) ++ "\n" ++
  "FORTUNE: " ++ show(FORTUNE) ++ "\n"
] ++ ["\n"];