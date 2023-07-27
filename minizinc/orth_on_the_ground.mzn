% 
% Global constraint orth_on_the_ground in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Corth_on_the_ground.html
% """
% Constraint
%
%     orth_on_the_ground(ORTHOTOPE,VERTICAL_DIM)
%
% Purpose
%
%     The ori attribute of the VERTICAL_DIMth item of the ORTHOTOPES 
%     collection should be fixed to one.
%
% Example
%     (<ori-1 siz-2 end-3,ori-2 siz-3 end-5>,1)
%
%     The orth_on_the_ground constraint holds since the ori attribute of 
%     its 1th item <ori-1 siz-2 end-3> (i.e., 1th item since 
%     VERTICAL_DIM=1) is set to one.
%
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 2;
array[1..n, 1..3] of var 1..5: orthotope;
var 1..n: vertical_dim;

predicate orth_on_the_ground(array[int,int] of var int: orthotope,
                             var int: vertical_dim) =
     orthotope[vertical_dim,1] = 1
;

predicate cp2d(array[int,int] of var int: x, array[int,int] of var int: y) =
  assert(index_set_1of2(x) = index_set_1of2(y) /\
         index_set_2of2(x) = index_set_2of2(y),
           "cp2d: x and y have different sizes",
     forall(i in index_set_1of2(x), j in index_set_2of2(x)) (
         y[i,j] = x[i,j]
    ) 
  )
; 

solve satisfy;

constraint
  cp2d(orthotope, array2d(1..n, 1..3, [ 
                     1,2,3,
                     2,3,5
                     ]))
  /\
  vertical_dim = 1 
  /\
  orth_on_the_ground(orthotope, vertical_dim)


;

output
[
  show(orthotope)
];
