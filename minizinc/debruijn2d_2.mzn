% 
% de Bruijn torus/arrays/matrices in Minizinc.
%
% This is a 2D version of de Bruijn sequences, i.e.
% de Bruijn matrices/arrays.
%
% See for example:
%   - Glenn Hurlbert, Garth Isaak: "On the de Bruijn Torus problem"
%
%
% This version use an extra column/row which mirrors
% the first column/row to simplify the calculations,
% which allows for arbitrary size of sub matrices.
%
% Example: a 4x4 grid with 2x2 sub grids in base 2.
%
%   a,b,c,d,
%   e,f,g,h,
%   i,j,k,l,
%   m,n,o,p,
% 
% where a..p can be either 0 or 1 (i.e. base 2).
% 
% The de Bruijn property is that we can move a 
% sub matrix of size 2 x 2 over this grid, i.e.
% sub grids such as 
%   ab  bc  cd  
%   ef  fg  gh
% 
% and around the corners
%   da
%   he
% 
% These will be interpreted as the numbers in base 2, i.e.
% as the following numbers (base 2):
% 
%    abef, bcfg, cdgh, and dahe
% 
% There are 16 such sub grids in this setup:
% 
%  abef, bfcg, cgdh, dahe,
%  efij, fgjk, ghkl, heli,
%  ijmn, jkno, klop, lipm,
%  mnab, nobc, opcd, omda
%
% Note the wrap around in both rows and columns.
% 
% To be able to ensure that the sub grids are distinct
% we use the converted subgrids and require that these
% 16 (converted) numbers must be distinct, using the 
% all_different constraint, of course.
%
% 
% For easier handling the wrap around, we use an expanded 
% grid where the first row and column are repeated in 
% this (dim+1) x (dim+1) grid:
% 
%   a,b,c,d,a  
%   e,f,g,h,e
%   i,j,k,l,i
%   m,n,o,p,m
%   a,b,c,d,a
%
%
% Here is an example of a 4x4 matrix with 2x2 sub grids in base 2:
% 
%
% x: [1, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1]
% x (dim x dim):
% 1 0 1 1
% 1 0 0 0
% 0 0 0 1
% 1 1 0 1
%
% x expanded (dim+1 x dim+1):
% 1 0 1 1 1
% 1 0 0 0 1
% 0 0 0 1 0
% 1 1 0 1 1
% 1 0 1 1 1
%
% x as m-tuples:
% [1, 0, 1, 0] y[0]: 10
% [0, 1, 0, 0] y[1]: 4
% [1, 1, 0, 0] y[2]: 12
% [1, 1, 0, 1] y[3]: 13
% [1, 0, 0, 0] y[4]: 8
% [0, 0, 0, 0] y[5]: 0
% [0, 0, 0, 1] y[6]: 1
% [0, 1, 1, 0] y[7]: 6
% [0, 0, 1, 1] y[8]: 3
% [0, 0, 1, 0] y[9]: 2
% [0, 1, 0, 1] y[10]: 5
% [1, 0, 1, 1] y[11]: 11
% [1, 1, 1, 0] y[12]: 14
% [1, 0, 0, 1] y[13]: 9
% [0, 1, 1, 1] y[14]: 7
% [1, 1, 1, 1] y[15]: 15
%
% y is the 2x2 sub grids converted to integers by
% converting the array [a,b,c,d] to the integer
%
%    abcd
%
% in the selected base
%
%   a,b,_,_
%   c,d,_,_
%   _,_,_,_
%   _,_,_,_
% 
% 
% y: [10, 4, 12, 13, 8, 0, 1, 6, 3, 2, 5, 11, 14, 9, 7, 15]
%
% 10  4 12 13
%  8  0  1  6
%  3  2  5 11
% 14  9  7 15
%
%
%
% And here is a 10 x 10 de Bruijn matrix with 2x2 sub grids, in base 10
%
% 0 0 0 0 0 0 0 0 0 0
% 0 0 1 1 2 0 2 3 2 1
% 0 3 0 0 0 0 0 0 0 0
% 0 0 0 4 1 4 3 3 1 3
% 0 5 0 0 0 0 0 0 0 1
% 0 0 0 6 0 7 0 8 0 1
% 0 9 0 0 0 0 0 0 0 2
% 0 0 1 5 1 6 1 7 1 1
% 1 8 0 0 0 1 0 0 0 1
% 4 0 1 9 1 3 4 2 2 4
%
% With this corresponding y matrix:
%
%    0    1   11   12   20    2   23   32   21   10
%    3  130 1100 1200 2000  200 2300 3200 2100 1000
%  300 3000    4   41   14   43   33   31   13   30
%    5   50  400 4100 1400 4300 3300 3100 1301 3010
%  500 5000    6   60    7   70    8   80  101 1010
%    9   90  600 6000  700 7000  800 8000  102 1020
%  900 9001   15   51   16   61   17   71  211 2010
%   18  180 1500 5100 1601 6110 1700 7100 1101 1011
% 1840 8001   19   91  113 1034   42   22  124 1144
% 4000  100 1900 9100 1300 3400 4200 2200 2400 4400
%
%

% And here is a 10x10 grid with subgrid 3x3 where
% all rows/columns/diagonals are different.
% (Though not all subgrids are distinct.)
% 
%   0 1 2 3 4 5 6 7 8 9
%   5 2 4 0 3 7 8 6 9 1
%   8 4 7 6 0 9 3 1 2 5
%   9 5 6 8 1 0 2 4 7 3
%   7 8 3 1 9 4 5 2 0 6
%   1 6 0 5 2 3 7 9 4 8
%   4 7 5 9 6 2 1 8 3 0
%   3 0 9 7 8 6 4 5 1 2
%   2 3 8 4 5 1 9 0 6 7
%   6 9 1 2 7 8 0 3 5 4
%
% Here is a 9x9 grid, base 9 with subgrid 3x3 and 
% the following additional constraints:
%
%   - all rows and columns are distinct
%   - the two main diagonal are distinct
%   - the 9 "Sudoku sub grids" are distinct
% 
%   0 1 2 3 4 5 6 7 8
%   4 5 8 2 6 7 0 3 1
%   3 6 7 0 1 8 4 2 5
%   7 2 1 8 0 6 5 4 3
%   6 0 5 4 2 3 8 1 7
%   8 4 3 5 7 1 2 0 6
%   1 7 0 6 5 4 3 8 2
%   5 3 4 1 8 2 7 6 0
%   2 8 6 7 3 0 1 5 4
% 
%  Here is a variant of the "Sudoku" with base 10 where we require
%  that all cells > 0
%
%    5 2 9 7 1 6 8 4 3
%    7 3 4 5 8 9 2 1 6
%    6 1 8 4 2 3 5 9 7
%    9 7 3 2 5 8 1 6 4
%    4 8 1 6 9 7 3 5 2
%    2 6 5 1 3 4 7 8 9
%    1 9 7 8 4 2 6 3 5
%    3 5 2 9 6 1 4 7 8
%    8 4 6 3 7 5 9 2 1

%
% 
% 
% TODO: Now it's a square matrix. Make it just rectangular
%       i.e. a n x m matrix (though still square submatrices)
% 


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%


include "globals.mzn"; 

% the dimension of the matrix (dim x dim)
int: dim = 9;
int: dim2 = dim*dim;

% the base to use, i.e. the alphabet 0..base-1
int: base = 9;

% dimension of the sub squares (m x m)
int: m = 3;

% decision variables 
% 
% extra: number of added row(s)/column(s)
int: extra = m-1;
% the matrix
array[0..dim-1+extra, 0..dim-1+extra] of var 0..base-1: x;

% convert the sub matrix to numbers
int: sub_max = ceil(pow(int2float(base), int2float(m*m)))-1;
array[0..dim*dim-1] of var 0..sub_max: y;

% count of the occurrences of numbers in y
% array[0..base-1] of var 0..dim2: gcc;

% var 0..sub_max: max_val;


% convert the array a to number n in base base
predicate toNum(array[int] of var int: a, var int: n, int: base) =
   let { int: len = length(a) }
   in
   n = sum(i in 1..len) (
      ceil(pow(int2float(base), int2float(len-i))) * a[i]
   )
;


solve :: int_search(
        [x[i,j] | i,j in 0..dim] ++ y, 
        first_fail, 
        indomain_min,
        complete) 
    % maximize(sum(y));
    % minimize(sum(y));
    % minimize max_val;
    satisfy;

% solve satisfy;


%
% the constraints
%
constraint
  trace("base: " ++ show(base) ++ 
        " dim: " ++ show(dim) ++ 
        " m: " ++ show(m) ++ 
        " extra: " ++ show(extra) ++
        " sub_max: " ++ show(sub_max) ++
        "\n", 1=1)
;


% mirror first -> last columns/rows
constraint
   forall(i in 0..dim-1+extra) (
      forall(j in 0..extra-1) (
        x[i,dim+j] = x[i,j]
         /\
        x[dim+j,i] = x[j,i] 
      )
   )
;

constraint

  forall(i, j in 0..dim-1) (
     % % convert 2x2 sub grid to an integer
     % toNum([x[i  , j], 
     %        x[i  , j+1], 
     %        x[i+1, j], 
     %        x[i+1, j+1]
     %       ], y[i*dim+j], base)
     % /\
     toNum([ x[i+k1, j+k2] | k1, k2 in 0..m-1], y[i*dim+j], base)

  )

  /\ % now we ensure that the 
     % converted sub grids are unique
     % Note: using ":: domain" may require very much 
     % memory for larger problems. So don't.
  alldifferent(y) % :: domain


  % extra constraints 
  % /\ global_cardinality_old([x[i,j] | i,j in 0..dim-1], gcc)
  % global_cardinality([x[i,j] | i,j in 0..dim-1], set2array(0..base-1),  gcc)

  % /\
  % forall(i in 1..base-1) (
  %     gcc[i-1] = gcc[i]
  % )

  % /\ % experimental
  % forall(i in 0..dim2-1) (
  %     exists(k in 0..dim2-1) (
  %        y[k] = i
  %     ) 
  % )
 
  % symmetry breaking
  % /\ y[0] = 0
;

% constraint maximum(max_val, y);

% require that all values > 0
% constraint forall(i,j in 0..dim-1) ( x[i,j] > 0 );

% require that all rows/columns (0..dim-1) are distinct
% (and - perhaps - diagonals)
% int: reg = ceil(sqrt(int2float(dim))); % size of the Sudoku regions
% constraint
%   forall(i,j in 0..dim-1) ( 
%      alldifferent([x[i,j] | j in 0..dim-1])
%      /\
%      alldifferent([x[j,i] | j in 0..dim-1])
%   )
%   %/\ % all subgrids are distinct (very experimental)
%   %forall(i,j in 0..dim-1) (
%   %   alldifferent([ x[i+k1, j+k2] | k1, k2 in 0..m-1])
%   %)

%   /\ % diagonal
%   alldifferent([x[i,i] | i in 0..dim-1])
%   /\ % diagonal
%   alldifferent([x[i,dim-i] | i in 0..dim-1])  

%   % Require Sudoku sub grids
%   /\
%   forall(i in 0..reg-1,j in 0..reg-1) (
%     % trace(show([1+r*dim+c | r in i*reg..(i*reg)+reg-1, c in j*reg..(j*reg)+reg-1]) ++ "\n", 1=1)
%     % /\
%     alldifferent([x[r,c] | r in i*reg..(i*reg)+reg-1, c in j*reg..(j*reg)+reg-1])
%   )
% ;


%
% A lot of information is printed.
%
output
[
 "x: " ++ show([x[i,j] | i,j in 0..dim-1]) ++ "\n"
]
++
["x (dim x dim):" ]
++
[
   if j = 0 then "\n" else " " endif ++
     show(x[i,j])
   | i,j in 0..dim-1
] 
++
["\n\nx expanded (dim-1+extra x dim-1+extra):" ]
++
[
   if j = 0 then "\n" else " " endif ++
     show(x[i,j])
   | i,j in 0..dim-1+extra
] 
++
[
  "\n\nx as m-tuples:\n"
]
++
[
  show([ x[i+k1, j+k2] | k1,k2 in 0..m-1]) ++  
       " y[" ++ show(i*dim+j) ++ "]: " ++ show(y[i*dim+j]) ++  "\n"
  | i,j in 0..dim-1
] 
++
["\n\ny: " ++ show(y) ++ "\n"]
++
[
  if j = 0 then "\n" else " " endif ++
    if fix(y[i*dim+j]) < 10 then " " else "" endif ++
      show(y[i*dim+j])
  | i,j in 0..dim-1
]
% ++ ["\n\ngcc: " ++ show(gcc) ++ "\n" ]
% ++ ["\n\nmax_val: " ++ show(max_val) ++ "\n" ]
++
["\n"]
;

        

% All 16 valid 2x2 squares
% K: +allcomb . 4 2
% array[1..dim*dim, 1..m*m] of int: t = array2d(1..dim*dim, 1..m*m, 
% [
% 0,0,0,0,
% 0,0,0,1,
% 0,0,1,0,
% 0,0,1,1,
% 0,1,0,0,
% 0,1,0,1,
% 0,1,1,0,
% 0,1,1,1,
% 1,0,0,0,
% 1,0,0,1,
% 1,0,1,0,
% 1,0,1,1,
% 1,1,0,0,
% 1,1,0,1,
% 1,1,1,0,
% 1,1,1,1,
% ]);


