% 
% Global constraint alldifferent_cst in MiniZinc.
%
% https://sofdem.github.io/gccat/gccat/Calldifferent_cst.html
% """
% For all pairs of items​(VARIABLES[i], VARIABLES[j]) (i!=j) of the 
% collection VARIABLES enforce 
% VARIABLES​[i].var+VARIABLES​[i].cst != VARIABLES​[j].var+VARIABLES[j].cst.
%
% 
% Example
%  (<
%     var−5 cst−0,
%     var−1 cst−1,
%     var−9 cst−0,
%     var−3 cst−4
%  >
%  )
% 
% The alldifferent_cst constraint holds since all the expressions 
% 5+0=5, 1+1=2, 9+0=9 and 3+4=7 correspond to distinct values.
% """

% 
% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc

include "globals.mzn"; 

int: n;
array[1..n] of var 1..9: x;
array[1..n] of int: cst;

solve satisfy;

%
% all_different_cst for +
%
predicate all_different_cst(array[int] of var int: variables, array[int] of int: ccst) =
   let {
      int: lbx = min(index_set(variables)),
      int: ubx = max(index_set(variables)),
      array[lbx..ubx] of var int: z1 = [variables[i] + ccst[i] | i in index_set(variables)]
   }
   in
   % forall(i,j in index_set(z1) where i != j) (
   %    z1[i] != z1[j]
   % )
   all_different(z1)
;


%
% all_different_cst for * (experimental)
%
predicate all_different_cst_mult(array[int] of var int: variables, array[int] of int: ccst) =
   let {
      int: lbx = min(index_set(variables)),
      int: ubx = max(index_set(variables)),
      array[lbx..ubx] of var int: z2 = [variables[i] * ccst[i] | i in index_set(x)]
   }
   in
   forall(i,j in index_set(z2) where i != j) (
       z2[i] != z2[j]
   )
;

% copy 
predicate cp1d(array[int] of int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
    forall(i in index_set(x)) (
       x[i] = y[i]
    )
  )
; 


constraint
   % cp1d([5,1,9,3],x)
   % /\
   all_different_cst(x, cst)
   %/\
   % all_different_cst_mult(x, cst)

;

%
% data
%
n = 4;
cst = [0,1,0,4];
% cst = [1,2,1,4]; % for test of alldifferent_cst_mult


output [
  "x  : " ++ show(x) ++ "\n" ++
  "cst: " ++ show(cst) ++ "\n"

] ++ ["\n"];
