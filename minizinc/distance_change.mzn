% 
% Global constraint distance change in MiniZinc.
% 
% From Global Constraint Catalogue:
% https://sofdem.github.io/gccat/gccat/Cdistance_change.html
% """
% Constraint
% 
%     distance_change​(DIST,​VARIABLES1,​VARIABLES2,​CTR)​
% 
% ...
% Purpose

%     DIST is equal to the number of times one of the following two 
%     conditions is true (1<=i<n):
%         * VARIABLES1[i].var CTR VARIABLES1[i+1].var holds and 
%           VARIABLES2[i].var CTR VARIABLES2[i+1].var does not hold,
%         * VARIABLES2[i].var CTR VARIABLES2[i+1].var holds and 
%           VARIABLES1[i].var CTR VARIABLES1[i+1].var does not hold.
% Example
%     (
%     1, <3, 3, 1, 2, 2>,
%        <4, 4, 3, 3, 3>,​!=
%     )

%     The distance_change constraint holds since the following 
%     condition (DIST=1) is verified: {
%       VARIABLES1[3].var=1 != VARIABLES1[4].var=2 /\
%       VARIABLES2[3].var=3 = VARIABLES1[4].var=3
%     }
% """
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

% include "globals.mzn"; 


int: n = 5;
array[1..n] of var 1..3: v1;
array[1..n] of var 1..4: v2;
var 0..n: dist;
int: ctr = 4; % !=

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

predicate distance_change(var int: dist, 
                         array[int] of var int: var1, 
                         array[int] of var int: var2, 
                         int: ctr) =
   let {
     int: n = length(var1),
     int: lbv = min(index_set(var1)),
     int: ubv = max(index_set(var1)),
   }
   in
   dist = sum(i in lbv..ubv-1) (
      bool2int(
            (cmp(var1[i],var1[i+1], ctr) /\ not(cmp(var2[i],var2[i+1], ctr)))
            \/
            (cmp(var2[i],var2[i+1], ctr) /\ not(cmp(var1[i],var1[i+1], ctr)))
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

   cp1d(v1, [3, 3, 1, 2, 2])
   /\
   cp1d(v2,[4, 4, 3, 3, 3])
    /\
   distance_change(dist, v1, v2, ctr)
;

output
[
  "dist: " ++ show(dist) ++ "\n" ++
  "v1: " ++ show(v1) ++ "\n" ++
  "v2: " ++ show(v2) ++ "\n" ++  
  "ctr: " ++ show(ctr) ++ "\n"
 
];
