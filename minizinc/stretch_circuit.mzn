% 
% Global constraint stretch_circuit in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/sec4.273.html
% """
% Let n be the number of variables of the collection VARIABLES and let i, j ​
% (0<=i​<n,​0<=j​<n)​be two positions within the collection of variables VARIABLES 
% such that the following conditions apply:
%
% * If i<=j then all variables Xi,​...,​Xj take a same value from the set of values of the 
% val attribute.
%   If i>​j then all variables Xi,​...,​Xn-1,​X0,​...,​Xj take a same value from the 
%   set of values of the val attribute.
%
%  * X(i-1)​mod n is different from Xi.
%
%  * X(j+1)​mod n is different from Xj.
%
% We call such a set of variables a stretch. The span of the stretch is equal 
% to 1 +​(j-i) mod n, while the value of the stretch is Xi. An item (val-v,​lmin-s,​lmax-t)
% enforces the minimum value s as well as the maximum value t for the span of a stretch of value v.
%
% Example
%     (
%     <
%     var-6,​
%     var-6,​
%     var-3,​
%     var-1,​
%     var-1,​
%     var-1,​
%     var-6,​
%     var-6
%     >,​
%     <
%     val-1	lmin-2	lmax-4,​
%     val-2	lmin-2	lmax-3,​
%     val-3	lmin-1	lmax-6,​
%     val-6	lmin-2	lmax-4
%     >
%     )
%
% The stretch_circuit constraint holds since the sequence 6 6 3 1 1 1 6 6 
% contains three stretches 6 6 6 6, 3, and 1 1 1 respectively verifying the following conditions:
%
% * The span of the first stretch 6 6 6 6 is located within interval ​[2,​4]​ 
%   (i.e., the limit associated with value 6).
% * The span of the second stretch 3 is located within interval ​[1,​6]​ 
%   (i.e., the limit associated with value 3).
% * The span of the third stretch 1 1 1 is located within interval ​[2,​4]​ 
%   (i.e., the limit associated with value 1).
% """

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 

int: n = 8; % length of sequence
int: m = 4; % length of stretch constraints

set of int: S = {1,2,3,6};
array[1..n] of var S: x;
array[1..m] of var S: val;
array[1..m] of var 1..n: lmin;
array[1..m] of var 1..n: lmax;

predicate stretch_circuit(array[int] of var int: x, array[int] of var int: val, array[int] of var int: lmin, array[int] of var int: lmax) =

  forall(s in 1..m) (
    lmin[s] <= lmax[s]
  )
  /\
  forall(s in 1..m) (

    % there may be no val[s] in x 
    not exists(k in 0..n-1) (x[k] = val[s]) 
    \/
    exists(i,j in 0..n-1) (
      % checking "this" value of val[s]
      x[i] = val[s]

      /\ % check the valid intervals
      (1 + (j-i)) mod n >= lmin[s]  
      /\ 
      (1 + (j-i)) mod n <= lmax[s] 

      /\ % the stretches
      if i<= j then
         forall(k in i+1..j) (
           x[k] = x[k-1]
         )
      else 
         x[i] = x[j]
         /\
         forall(k in i+1..n-1) (
           x[k] = x[k-1]
         )
         /\
         forall(k in 1..j) (
           x[k] = x[k-1]
         )
      endif

      /\
      x[(i-1) mod n] != x[i] 
      /\
      x[(j+1) mod n] != x[j]

    )
  )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


solve satisfy;

constraint
  % cp1d(x, array1d(1..n, [6,6,3,1,1,1,6,6]))
  % /\
  cp1d(val,  [1,2,3,6])
  /\
  cp1d(lmin, [2,2,1,4])
  /\
  cp1d(lmax, [4,3,6,4])
  /\
  stretch_circuit(x, val, lmin, lmax)
;


output [
"x: " ++ show(x) ++ "\n"++
"val: " ++ show(val) ++ "\n"++
"lmin: " ++ show(lmin) ++ "\n"++
"lmax: " ++ show(lmax) ++ "\n"
];
