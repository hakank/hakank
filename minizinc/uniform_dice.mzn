% 
% Uniform Dice in MiniZinc.
% 
% This model generates two dice with uniform distribution of
% the sums.
%
% Example: for n = 6, m = 6:
% 
%   dist: [0, 0, 0, 0, 0, 0, 6, 6, 6, 6, 6, 6]
%   x1[2, 2, 4, 4, 6, 6]
%   x2[5, 5, 5, 6, 6, 6]
%    7: 6
%    8: 6
%    9: 6
%   10: 6
%   11: 6
%   12: 6
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
% 

include "globals.mzn"; 
int: n = 6;
int: m = 10; % max integer 


% the two dice
array[1..n] of var 1..m: x1;
array[1..n] of var 1..m: x2;
array[1..m*2] of var 0..n*n: dist;
var 0..m*2: z = sum(k in 1..m*2) (bool2int(dist[k] > 0));


% solve satisfy;
solve :: int_search(dist ++ x1 ++ x2, smallest, indomain_min, complete) satisfy;

% same_except_0: either a value is 0 or is the same values
predicate same_except_0(array[int] of var int: x) =
   forall(k in 1..length(x)-1) (
       x[k] = 0 \/ (x[k] = x[k+1])
   )
;

constraint
 
   forall(k in 1..m*2) (
       dist[k] = sum(i,j in 1..n) ( bool2int(x1[i]+x2[j] == k))
   )

   % redundant constraints
   % /\ n*n mod dist[m*2] = 0
   % /\ n*n mod z = 0
   /\ sum(dist) = n*n

   % /\ increasing(dist)
   /\
   %forall(k in 1..m*2-1) (
   %    dist[k] = 0 \/ (dist[k] = dist[k+1])
   %)
   same_except_0(dist)
   
   % symmetry breaking
   /\ increasing(x1) 
   /\ increasing(x2)
   /\ lex_lesseq(x1,x2)
  
   % /\ x1[1] = 1

 
;


output 
[
  "z   : " ++ show(z) ++ "\n" ++
  "dist: " ++ show(dist) ++ "\n" ++
  "x1  : " ++ show(x1) ++ "\n" ++
  "x2  : " ++ show(x2) ++ "\n" 
]
++
[
  if fix(dist[i]) > 0 then
     show_int(2, i) ++ ": " ++ show(dist[i]) ++ "\n"
  else 
    ""
  endif
  | i in 1..m*2
];
