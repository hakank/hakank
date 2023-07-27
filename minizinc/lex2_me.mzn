% 
% Global constraint lex2 in MiniZinc.
% 
% From Global Constraint Catalogue
% https://sofdem.github.io/gccat/gccat/Clex2.html
% """
% lex2​(MATRIX)
% 
% Purpose
%
% Given a matrix of domain variables, enforces that both adjacent rows, 
% and adjacent columns are lexicographically ordered (adjacent rows and 
% adjacent columns can be equal).
%
% Example
%     (
%     <
%     vec-<2, 2, 3>,
%     vec-<2, 3, 1>
%     >
%     )
%
% The lex2 constraint holds since:
%
% * The first row <2, 2, 3> is lexicographically less than or equal to the 
%   second row <2, 3, 1>.
% * The first column <2, 2> is lexicographically less than or equal to the 
%   second column <2, 3>.
% * The second column <2, 3> is lexicographically less than or equal to the 
%   third column <3, 1>.
%
% """


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: r = 2;
int: c = 3;
array[1..r, 1..c] of var 1..4: x;

predicate lex2_me(array[int, int] of var int: x) =
  let {
    int: lbx1 = min(index_set_1of2(x)),
    int: ubx1 = max(index_set_1of2(x)),
    int: lbx2 = min(index_set_2of2(x)),
    int: ubx2 = max(index_set_2of2(x))

  } in
 forall(i in lbx1+1..ubx1) (
     lex_less( [x[i-1, j] | j in index_set_2of2(x)], [x[i, j] | j in index_set_2of2(x)])
  )
  /\
  forall(j in lbx2+1..ubx2) (
     lex_less( [x[i, j-1] | i in index_set_1of2(x)], [x[i, j] | i in index_set_1of2(x)])
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

% solve satisfy;
solve :: int_search([x[i,j] | i in 1..r, j in 1..c], first_fail, indomain_min, complete) satisfy;

constraint
  cp2d(x, array2d(1..r, 1..c, 
     [
       2,2,3,
       2,3,1
     ]))
  /\
  lex2_me(x)
;

output [
 if j = 1 then "\n" else " "  endif ++
   show(x[i,j])
 | i in 1..r, j in 1..c
] ++ ["\n"];
