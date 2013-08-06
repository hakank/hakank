% 
% Magic square 4 x 4 in MiniZinc.
% 
% Standard Prolog benchmark (BProlog etc)
% """ 
%   File   : magic4.pl
%   Author : Neng-Fa ZHOU
%   Date   : 1992
%   Purpose: solve the magic square puzzle for a 4*4 board
% """


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

set of int: r = 1..16;
var r: X1;
var r: X2;
var r: X3;
var r: X4;
var r: X5;
var r: X6;
var r: X7;
var r: X8;
var r: X9;
var r: X10;
var r: X11;
var r: X12;
var r: X13;
var r: X14;
var r: X15;
var r: X16;

array[1..16] of var r: Vars=[X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16];

% solve satisfy;
solve :: int_search(Vars, first_fail, indomain_min, complete) satisfy;

constraint
    all_different(Vars) /\

    X1 + X2 + X3 + X4 = 34 /\
    X5 + X6 + X7 + X8 = 34 /\
    X9 + X10 + X11 + X12 = 34 /\
    X13 + X14 + X15 + X16 = 34 /\

    X1 + X5 + X9 + X13 = 34 /\
    X2 + X6 + X10 + X14 = 34 /\
    X3 + X7 + X11 + X15 = 34 /\
    X4 + X8 + X12 + X16 = 34 /\

    X1 + X6 + X11 + X16 = 34 /\
    X4 + X7 + X10 + X13 = 34

;

output
[
 show(Vars)
];
