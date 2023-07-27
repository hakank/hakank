% 
% Global constraint lex_alldifferent in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Clex_alldifferent.html
% """
% lex_alldifferent​(VECTORS)
%
% Purpose
% 
% All the vectors of the collection VECTORS are distinct. Two vectors 
% (u1, u2, ..., un) and (v1, v2, ..., vn)​ are distinct if and only if 
% there exists i in​[1,​n]​ such that ui != vi.
% 
% Example
%   (
%   <
%    vec−〈5,​2,​3〉,​
%    vec−〈5,​2,​6〉,​
%    vec−〈5,​3,​3〉
%   >
%   )
%
% The lex_alldifferent constraint holds since:
%  * The first vector <5, 2, 3> and the second vector <5, 2, 6> of the 
%    VECTORS collection differ in their third component (i.e., 3 != 6).
%  * The first vector <5, 2, 3> and the third vector <5, 3, 3> of the 
%    VECTORS collection differ in their second component (i.e., 2 != 3).
%  * The second vector <5, 2, 6> and the third vector <5, 3, 3> of the 
%    VECTORS collection differ in their second and third components 
%    (i.e., 2!=3 and 6!=3).
% """


%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: n = 3;
int: m = 3;
array[1..m, 1..n] of var 1..6: x;


predicate lex_alldifferent(array[int,int] of var int: x) =
   forall(i, j in index_set_1of2(x) where i < j) (
        lex_less([x[i,k] | k in index_set_2of2(x) ], [x[j,k] | k in index_set_2of2(x)])
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
   cp2d(x, array2d(1..m, 1..n, 
      [
         5,2,3,
         5,2,6,
         5,3,3
      ]))
   /\
   lex_alldifferent(x)
;


output
[
  show(x)
];
