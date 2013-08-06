% 
% Enigma puzzle 1476: Portuguese squares in MiniZinc.
% 
% Problem formulation from 
% http://www.f1compiler.com/samples/Enigma%201476.f1.html
% """
% Zero, um, nove and cme are Portuguese for 0,1,9, and 100; so it is 
% appropriate that I can make the folowing statement: 
%      ZERO,UM,NOVE and CEM are perfect squares.
% In this statement digits have been consistently replaced by capital letters,
% different letters being used for different digits. No number starts with a 
% zero.
%
% Calculate the numerical value of square root of 
%      (ZERO*UM*NOVE*CEM)
%
% """
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% Note: MiniZinc/flatzinc gives integer overflow error.

include "globals.mzn"; 

set of int: INT = 0..9;
var INT: Z;
var INT: E;
var INT: R;
var INT: O;
var INT: U;
var INT: M;
var INT: N;
var INT: V;
var INT: C;

array[1..9] of var INT: x = [Z,E,R,O,U,M,N,V,C];

var 1..1000: x1;
var 1..10  : x2;
var 1..1000: x3;
var 1..100 : x4;


var int: square_root;
 
solve satisfy;

constraint
   all_different(x)
   /\
   x1*x1 = Z*1000+E*100+R*10+O  /\ Z >= 1 /\  
   x2*x2 =              U*10+M  /\ U >= 1 /\
   x3*x3 = N*1000+O*100+V*10+E  /\ N >= 1 /\
   x4*x4 =        C*100+E*10+M  /\ C >= 1 /\
   square_root = x1*x2*x3*x4 
;



output [
 "square_root: ", show(square_root), "\n",
 "x: ", show(x), "\n",

];