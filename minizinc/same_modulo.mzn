% 
% Global constraint same_modulo in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Csame_modulo.html
% """
% same_modulo(VARIABLES1,VARIABLES2,M)
%
% Purpose
% 
% For each integer R in [0,M1], let N1R (respectively N2R) denote 
% the number of variables of VARIABLES1 (respectively VARIABLES2) that 
% have R as a rest when divided by M. For all R in [0,M1] we have 
% that N1R=N2R.

% Example
%     (
%     <1,9,1,5,2,1>,
%     <6,4,1,1,5,5>,3
%     )
%
% The values of the first collection 1,9,1,5,2,1 are respectively 
% associated with the equivalence classes 1 mod 3=1, 9 mod 3=0, 1 mod 3=1, 
% 5 mod 3=2, 2 mod 3=2, 1 mod 3=1. Therefore the equivalence classes 
% 0, 1, and 2 are respectively used 1, 3, and 2 times. Similarly, the values 
% of the second collection <6,4,1,1,5,5> are respectively associated 
% with the equivalence classes 6 mod 3=0, 4 mod 3=1, 1 mod 3=1, 1 mod 3=1, 
% 5 mod 3=2, 5 mod 3=2. Therefore the equivalence classes 0, 1, and 2 are 
% respectively used 1, 3, and 2 times. Consequently the same_modulo 
% constraint holds. Figure 4.245.1 illustrates this correspondence. 
% """

%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: n = 6;
array[1..n] of var 1..9: v1;
array[1..n] of var 1..9: v2;
int: M = 3;

predicate same_modulo(array[int] of var int: v1, 
                      array[int] of var int: v2, 
                      int: M) =
  let {
    int: lbv = min(index_set(v1)),
    int: ubv = max(index_set(v1)),
    array[lbv..ubv] of var 0..M-1: m1,
    array[lbv..ubv] of var 0..M-1: m2,
    array[0..M-1] of var 0..n: gcc
  }
  in
  forall(i in index_set(v1)) (
     m1[i] = v1[i] mod M
     /\
     m2[i] = v2[i] mod M
  ) 
  /\
  global_cardinality(m1, array1d(0..M-1, set2array(index_set(gcc))), gcc) 
  /\
  global_cardinality(m2, array1d(0..M-1, set2array(index_set(gcc))), gcc) 
;

predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 

solve satisfy;

constraint
  cp1d(v1, [1,9,1,5,2,1])
  /\
  cp1d(v2, [6,4,1,1,5,5])
  /\
  same_modulo(v1, v2, M)
;

output
[
  "v1: " ++ show(v1) ++ "\n" ++
  "v2: " ++ show(v2) ++ "\n" ++
  "M: " ++ show(M) ++ "\n" 
];
