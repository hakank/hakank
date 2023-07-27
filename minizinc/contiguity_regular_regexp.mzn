% 
% Global constraint contiguity using regular regexp in MiniZinc.
% 
% This is the contiguity constraint using regular constraint.
%
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cglobal_contiguity.html
% """
% Enforce all variables of the VARIABLES collection to be assigned to 0 or 1. 
% In addition, all variables assigned to value 1 appear contiguously.
% 
% Example:
% (<0,​1,​1,​0>)
% 
% The global_contiguity constraint holds since the sequence 0 1 1 0 contains 
% no more than one group of contiguous 1.
% """
%
% Compare with http://hakank.org/minizinc/contiguity_regular.mzn
% which use the "standard" regular constraint.
%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 151;
array[1..n] of var 0..1: arr;


solve satisfy;

constraint 
  % regular(arr, "0*1+0*")   % require at least one 1
  regular(arr, "0*(0|1+)0*") % don't require at least 1
;

output [
   "arr : " ++ show(arr) ++ "\n" 

];
