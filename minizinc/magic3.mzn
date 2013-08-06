% 
% Magic square 3 x 3 in MiniZinc.
% 
% Standard prolog benchmark (BProlog etc)
% """ 
% File   : magic3.pl
% Author : Neng-Fa ZHOU
% Date   : 1992
% Purpose: solve the magic square puzzle for a 3*3 board
% """


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

set of int: r = 1..9;
var r: X1;
var r: X2;
var r: X3;
var r: X4;
var r: X5;
var r: X6;
var r: X7;
var r: X8;
var r: X9;
array[1..9] of var r: Vars = [X1,X2,X3,X4,X5,X6,X7,X8,X9];

% solve satisfy;
solve :: int_search(Vars, first_fail, indomain_min, complete) satisfy;

constraint
    all_different(Vars) /\

    X1 + X2 + X3 = 15 /\
    X4 + X5 + X6 = 15 /\
    X7 + X8 + X9 = 15 /\
    X1 + X4 + X7 = 15 /\
    X2 + X5 + X8 = 15 /\
    X3 + X6 + X9 = 15 /\
    X1 + X5 + X9 = 15 /\
    X3 + X5 + X7 = 15

;

output
[
  show(Vars)
];
