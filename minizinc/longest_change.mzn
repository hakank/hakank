% 
% Global constraint longest change in MiniZinc.
%
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Clongest_change.html
% """
% SIZE is the maximum number of consecutive variables of the collection 
% VARIABLES for which constraint CTR holds in an uninterrupted way. 
% We count a change when X CTR Y holds; X and Y are two consecutive 
% variables of the collection VARIABLES.
% 
% Example
% (
%  4, <
%     var-8,
%     var-8,
%     var-3,
%     var-4,
%     var-1,
%     var-1,
%     var-5,
%     var-5,
%     var-2>, 
%  !=)
% The longest_change constraint holds since its first argument SIZE=4 is 
% fixed to the length of the longest subsequence of consecutive values of 
% the collection <8, 8, 3, 4, 1, 1, 5, 5, 2> such that two consecutive 
% values are distinct (i.e., subsequence 8 3 4 1).
% """
% 

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 9;
array[1..n] of var 1..8: x;
var int: c;

%
% Since MiniZinc don't handle function variables we use the following 
% hack where t is the type of comparison operator.
% t:
%  - 2 :  a <  b
%  - 1 :  a <= b
%    0 :  a  = b
%    1 :  a >= b
%    2 :  a >  b
% else : a != b
%
predicate cmp(var int: a, var int: b, int: t) = 
   if     t = -2 then
     a < b
   elseif t = -1 then
     a <= b 
   elseif t =  0 then
     a = b
   elseif t =  1 then
     a >= b
   elseif t =  2 then
     a > b
   else 
     a != b
   endif
;


%
% Longest change.
%
% Uses two extra arrays.
% Well, this can probably be modelled more elegant...
% 
predicate longest_change(var int: c, array[int] of var int: x, int: ctr) =
   let {
        int: lbx = min(index_set(x)),
        int: ubx = max(index_set(x)),
        int: n = length(x),
        array[lbx+1..ubx] of var 0..1: t, 
        array[lbx+1..ubx] of var 0..ubx: cum_sum,
        var int: cc
   }
   in
     forall(i in lbx+1..ubx) (
       t[i] = 1 <-> cmp(x[i-1], x[i], ctr)
   )
   /\
   forall(i in lbx+2..ubx) (
     cum_sum[i] = t[i-1]*cum_sum[i-1] + t[i]
   )
   /\
   cum_sum[2] = 0
   /\
   maximum(cc, cum_sum)   
   /\
   c = cc + 1
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

solve satisfy;

constraint

   cp1d(x, [8, 8, 3, 4, 1, 1, 5, 5, 2])
   /\
   longest_change(c, x, 10) % 10 means !=

;


output
[
  "x: " ++ show(x) ++ "\n" ++
  "c: " ++ show(c) ++ "\n"
];
