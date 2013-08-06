% 
% Olympic puzzle in MiniZinc.
% 
% Benchmark for Prolog (BProlog)
% """
%   File   : olympic.pl
%   Author : Neng-Fa ZHOU
%   Date   : 1993
%
%   Purpose: solve a puzzle taken from Olympic Arithmetic Contest
%
%    Given ten variables with the following configuration:
%
%                X7   X8   X9   X10
%
%                   X4   X5   X6
%
%                      X2   X1             
%
%                         X1
%
%   We already know that X1 is equal to 3 and want to assign each variable
%   with a different integer from {1,2,...,10} such that for any three
%   variables 
%                       Xi   Xj
%
%                          Xk
%   the following constraint is satisfied:
%
%                     |Xi-Xj| = Xk
% """
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

set of int: r = 1..10;
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
array[1..10] of var r:  Vars=[X1,X2,X3,X4,X5,X6,X7,X8,X9,X10];


% solve satisfy;
solve :: int_search(Vars, first_fail, indomain_min, complete) satisfy;

% predicate minus(var int: x, var int: y, var int: z) =
%   (z = x - y) \/
%   (z = y - x)
%;

predicate minus(var int: x, var int: y, var int: z) =
   z = abs(x-y)
;


constraint

    all_different(Vars) /\
    X1 =3 /\
    minus(X2,X3,X1) /\
    minus(X4,X5,X2) /\
    minus(X5,X6,X3) /\
    minus(X7,X8,X4) /\
    minus(X8,X9,X5) /\
    minus(X9,X10,X6)

;

output [
  show(Vars), "\n"
];