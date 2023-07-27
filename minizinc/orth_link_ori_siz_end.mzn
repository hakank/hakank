% 
% Global constraint orth_link_ori_siz_end in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Corth_link_ori_siz_end.html
% """
% Constraint
%
%     orth_link_ori_siz_end​(ORTHOTOPE)​
%
% Purpose
%
%     Enforce for each item of the ORTHOTOPE collection the constraint 
%     ori+siz=end.
%
% Example
%     (<ori-2 siz-2 end-4, ori-1 siz-3 end-4>)
%
%     The orth_link_ori_siz_end constraint holds since the two items 
%     <ori-2 siz-2 end-4> and <ori-1 siz-3 end-4> respectively verify 
%     the conditions 2+2=4 and 1+3=4.
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n = 2;
array[1..n, 1..3] of var 1..4: orthotope;

predicate orth_link_ori_siz_end(array[int,int] of var int: orthotope) =
   forall(i in index_set_1of2(orthotope)) (
       orthotope[i,3] = orthotope[i,1] + orthotope[i,2]
   )
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
                          2,2,4,
                          1,3,4
                      ]))
   /\
   orth_link_ori_siz_end(orthotope)
;

output
[
 show(orthotope)
];
