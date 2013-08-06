% 
% Crossbar problem in MiniZinc.
%
% Prolog/FD benchmark problem (crossbar.pl in B Prolog)
% 
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 20;
set of int: r = 0..n;

var r: V1;
var r: V2;
var r: V3;
var r: V4;
var r: V5;
var r: V6;
var r: V7;
var r: V8;
var r: V9;
var r: V10;
var r: V11;
var r: V12;
var r: V13;
var r: V14;
var r: V15;
var r: V16;
var r: V17;
var r: V18;
var r: V19;
var r: V20;
array[1..n] of var r: List= [V1,V2,V3,V4,V5,V6,V7,V8,V9,V10,
                                V11,V12,V13,V14,V15,V16,V17,V18,V19,V20];


solve satisfy;
% solve :: int_search(x, "first_fail", "indomain", "complete") satisfy;

constraint
        V1 in {2,4,6,7,8,9,10,11,16,18,20} /\
	V2 in {2,3,4,8,10,12,17,19,20} /\
	V3 in {2,3,4,6,8,9,11,17,18} /\
	V4 in {1,3,4,5,6,7,9,10,11,13,18} /\
	V5 in {1,5,6,10,12,13,14,17,18,19,20} /\
	V6 in {1,3,10,12,15,16,19,20} /\
	V7 in {5,8,9,10,17} /\
        V8 in {1,2,5,6,7,12,14,15,16,17} /\
	V9 in {1,2,3,4,5,7,11,12,13,14,16,17,20} /\
	V10 in {4,5,8,9,10,11,13,17,18,19,20} /\
	V11 in {2,4,6,7,8,10,12,14,17,18,20} /\
	V12 in {3,7,8,9,10,13,14,15,18,20} /\
	V13 in {2,3,6,7,8,9,11,13,16,20} /\
	V14 in {2,3,5,6,8,9,12,13,15,16,17,18} /\
	V15 in {2,7,8,10,12,13,14,15,16,17,18,20} /\
	V16 in {1,2,6,11,13,16,17,19,20} /\
	V17 in {1,3,6,9,13,19} /\
	V18 in {1,3,6,7,8,10,13,14,19} /\
        V19 in {1,2,3,4,5,6,7,9,11,12,14,16,17,19,20} /\
        V20 in {3,5,6,7,8,9,11,12,13,14,16,18,20} /\
	all_different(List)
    
        % /\ % one solution
        % List = [2, 3, 4, 1, 5, 10, 8, 6, 7, 9, 12, 13, 11, 15, 16, 17, 19, 14, 20, 18]

;

output [
  show(List), "\n",
];
