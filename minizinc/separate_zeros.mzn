% 
% Separate zeros in MiniZinc.
%
% This is a general solution for the problem in
% "Minizinc array sorting"
% http://stackoverflow.com/questions/33856233/minizinc-array-sorting%
% """
% Lets say I have an array declaration looking like this
% array[1..5] of int: temp = [1,0,5,0,3];
%
% Is there a way to initiate a new array looking the same as temp but without the 0's? 
% The result would look like the following
% 
% [1,5,3]
% 
% or sort the array in such a way that the 0's would be either in the beginning or 
% in the end of the array, which would be
% 
% [0,0,1,5,3]
% 
% or
% 
% [1,5,3,0,0]
% """

%
% This model solves the second problem for decision variables. Note that
% this separation is stable, i.e. the values (!= 0) are in the same order
% as in the source array.
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn";

int: n = 5;
array[1..n] of var 0..6: s; % "original" array
array[1..n] of var lb_array(s)..ub_array(s): s2; % place zeros first
array[1..n] of var lb_array(s)..ub_array(s): s3; % place the zeros last

solve satisfy;

%
% count the number of not y in the array "x", from position "from" to position "to"
%
function var int: count_not_y(array[int] of var int: x, var int: y, int: from, int: to) =
   sum([x[j]!=y |j in from..to])
;

%
% count the number of y in the array "x", from position "from" to position "to"
%
function var int: count_y(array[int] of var int: x, var int: y, int: from, int: to) =
   sum([x[j]=y | j in from..to])
;

%
% the array "y" is a copy of the values in "x", except that the value "z" is placed first.
%
predicate separate_z_first(array[int] of var int: x, array[int] of var int: y, var int: z) =
   let {
     int: len = length(x)
   } in  
   forall(i in index_set(x)) (
      if x[i] != 0 then
        y[count_y(x,0,1,len) + count_not_y(x,0,1,i-1) +1 ] = x[i]
      else 
        y[count_y(x,0,1,i-1) + 1] = 0
      endif     
   )
;

%
% the array "y" is a copy of the values in "x", except that the value "z" is placed last.
%
predicate separate_z_last(array[int] of var int: x, array[int] of var int: y, var int: z) =
   let {
     int: len = length(x)
   } in
   forall(i in index_set(x)) (
      if x[i] != 0 then
        y[count_not_y(x,0,1,i-1) +1] = x[i]
      else 
        y[count_not_y(x,0,1,len) + count_y(x,0,1,i-1) +1] = 0
      endif
   )
;



constraint
  s = [1,0,5,0,3] /\
  separate_z_first(s,s2,0) /\
  separate_z_last(s,s3,0)
;

% let's having some fun :-)
% constraint
%   % require at least 2 zeros
%   sum([s[i] = 0 | i in 1..n]) >= 2 /\ 
%   separate_z_first(s,s2,0) /\
%   separate_z_last(s,s3,0)
% ;


output 
[
 "s: \(s)\n",
 "s2: \(s2)\n",
 "s3: \(s3)\n",
];