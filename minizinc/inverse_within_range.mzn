% 
% Global constraint inverse within range in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Cinverse_withing_range.html
% """
% inverse_within_range​(X,​Y)
% 
% Purpose
% If the ith variable of the collection X is assigned to j and if j is 
% less than or equal to the number of items of the collection Y then the 
% jth variable of the collection Y is assigned to i.
% 
% Conversely, if the jth variable of the collection Y is assigned to i and 
% if i is less than or equal to the number of items of the collection X 
% then the ith variable of the collection X is assigned to j.
% 
% Example
%   (
%     <9, 4, 2>,
%     <9, 3, 9, 2>
%   )
% 
% Since the second item of X is assigned to 4, the fourth item of Y is 
% assigned to 2. Similarly, since the third item of X is assigned to 2, the 
% second item of Y is assigned to 3. Figure 4.150.1 illustrates the 
% correspondence between X and Y.
% 
%     X     Y
%   
%   1 9    9 1
%   2 4    3 2
%   3 2    9 3  
%          2 4  
%    
%  Figure 4.150.1. Correspondence between the items of X=<9, 4, 2> and the 
% items of Y=<9, 3, 9, 2>
% 
% """


% Cf my model inverse_except_0.mzn.
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn";

int: n1 = 3;
int: n2 = 4;

array[1..n1] of var {1,2,3,4,9}: x1;
array[1..n2] of var {1,2,3,9}: x2;


predicate contains(var int: e, array[int] of var int: a) =
   exists(i in index_set(a)) (
      a[i] = e
   )
;

predicate inverse_within_range(array[int] of var int: x1, array[int] of var int: x2) = 

   forall(i in index_set(x1)) (
     x1[i] <= n2 <-> exists(j in index_set(x2)) (x2[j]  = i /\ x1[i] = j)
   )
   /\
   forall(j in index_set(x2)) (
     x2[j] <= n1 <->  exists(i in index_set(x1)) (x1[i]  = j /\ x2[j] = i)
   )
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 


solve satisfy;

constraint
  cp1d(x1, [9,4,2])
  /\
  cp1d(x2, [9,3,9,2])
  /\
  inverse_within_range(x1, x2)

   
;

output
[
  "x1: " ++ show(x1) ++ "\n" ++
  "x2: " ++ show(x2) ++ "\n" 
];
