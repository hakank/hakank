% 
% de Bruijn torus (arrays, matrices) in Minizinc.
%
% This is a 2D version of de Bruijn sequences, i.e.
% de Bruijn matrices/arrays/torus.
%
% See for example:
%   - Glenn Hurlbert, Garth Isaak: "On the de Bruijn Torus problem"
% 
% 
% Notation:
%  It seems that the convention is to code a de Bruijn torus
%  as (major row, major column, minor row, minor column)base
%  where 
%   - major row/column is the size of the matrix and
%   - minor row/column is the size of the sub matrices
%   - base (sub scripted)
% Here I use the following notation:
% (major row, major column, minor row, minor column, base)


%
% This version use an extra column/row which mirrors
% the first column/row to simplify the calculations,
% which allows for arbitrary size of sub matrices.
%
% This version also supports non-square matrices 
% and sub-matrices.
% 
% It has two alternative approaches:
% 
%  - Using the y array for calculating the number corresponding
%    to the tuples which allows us to constrain it by 
%    all_different(y).
%
%  - Using a "poor man's" alldifferent on the tuples via
%    a couple of !=.
% 
%  The advantage of using the y approach is that it "drives" 
%  the solution and propagates probably faster than the other
%  approach, as well as simplifies the flattening pre-process.
%
%  On the other hand, using y and alldifferent(y) may restrict
%  the range of dimensions since it's sensitive to the maximum
%  value that is acceptable by the MiniZinc or the solver.
%

% 
%
% Basic example: here is a 4x4 grid with 2x2 sub grids (in some base):
% This is notated as (4,4,2,2, base):
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
% (4,4, 2,2, 2)
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
% (10,10, 2,2, 10)
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

% Here's a 10x10 grid with subgrid 3x3 (10,10, 3,3, 10) where
% all rows/columns/diagonals are different.
% Though not all subgrids are distinct.
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
% Here is a 9x9 grid, base 9 with subgrid 3x3 
% (9,9, 3,3, 9)
% and the following additional constraints:
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
% Note: There is no solution if we also should require that 
% all subgrids must be distinct.
% 
% This is a variant of the "Sudoku" with base 10 (10,10, 3,3, 10)
% where we require that all cells > 0:
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
% This MiniZinc model allows us to do non-square matrices, so 
% here is a 2 x 26 grid with 2 x 2 sub grid, base 13
% (2,26, 2,2, 13)
% It has the additional constraints that the number of occurrances
% of each value in x is the same, i.e. there are four 0's, four 1's
% etc. (The gcc variable is used with the global constraint 
% global_cardinality_old.)
% 
%  6  3 12 10  1  8  9  8  6 11  4  2  5  8  3  3  4  2  7  4  8  0  1  1 12  5
% 10  9  3  9 12  5  7 12  7 11  9 11  1  2  0  2 10  6 11  4  7 10  5  0  6  0
% 
%
% Here is 2 x 26 with 2 x 1 sub grid, base 13 (2,26, 2,1, 13)
%
%  6  3 12 10  1  8  9  8  6 11  4  2  5  8  3  3  4  2  7  4  8  0  1  1 12  5
% 10  7  9  9  9 11  3  0  7  0  6 10 10  1 12  4 11  5 12  0  2  7  2  5 11  6
%
% The sub grid 2x1 yields these tuples: [6,10], [3,7], [12,9], ... [12,11], [5,6]. 
% Perhaps not very interesting...
%
%
% Statistics:
% Here are the number of solutions for some 
% of the simpler configurations:
%
%  (2,2  2,2, 2):        8 solutions
%  (4,4, 2,2, 2):       32 solutions
%  (2,2, 2,2, 3):       60 solutions
%  (3,3, 2,2, 2):      162 solutions
%  (2,2, 2,2, 4):      216 solutions
%  (2,2, 2,2, 5):      560 solutions
%  (2,2, 2,2, 6):     1200 solutions
%  (2,2, 2,2, 7):     2268 solutions
%  (2,2, 2,2, 8):     3920 solutions
%  (2,2, 2,2, 9):     6336 solutions
%  (2,2, 2,2, 10):    9720 solutions
%  (3,3, 2,2, 3):    15696 solutions
%  (3,3, 3,3, 3):    19584 solutions
%  (4,4, 3,3, 2):    54048 solutions
%  (3,3, 2,2, 4):   242964 solutions 
%  (4,4, 2,2, 3): 12953088 solutions
 


% 
%
% Note: There are often very many solutions, so the value selector 
% "indomain_random" can be used to avoid the "boring" solutions with many 
% 0's which tend to be the first solutions when using indomain_min.
% Also, adding the gcc constraint may generate more interesting solutions,
% depending on the application, of course.


% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%


include "globals.mzn"; 

% the dimension of the matrix (row x cols)
int: rows = 1;
int: cols = 64;

% the base to use, i.e. the alphabet 0..base-1
int: base = 2;

% dimensions of the sub squares (m1 x m2)
int: m1 = 1; % rows
int: m2 = 6; % cols

% decision variables 
% 
% extra: number of added row(s)/column(s)
int: extra1 = m1-1;
int: extra2 = m2-1;
% the matrix
array[0..rows-1+extra1, 0..cols-1+extra2] of var 0..base-1: x;

% convert the sub matrix to numbers
% int: sub_max = ceil(pow(int2float(base), int2float(m1*m2)))-1;
% array[0..rows*cols-1] of var 0..sub_max: y;

% count of the occurrences of numbers in y
% array[0..base-1] of var 0..rows*cols: gcc;

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
        [x[i,j] | i in 0..rows-1, j in 0..cols-1], % ++ y, 
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
        " rows: " ++ show(rows) ++ 
        " cols: " ++ show(cols) ++ 
        " m1: " ++ show(m1) ++ 
        " m2: " ++ show(m2) ++ 
        " extra1: " ++ show(extra1) ++
        " extra2: " ++ show(extra2) ++
        % " sub_max: " ++ show(sub_max) ++ 
        "\n", 1=1)
;


constraint
   assert(m1 <= rows /\ m2 <= cols,
         "m1 and m1 must be <= rows and cols")
;


%
% mirror first->last columns/rows
%
constraint
   forall(i in 0..rows-1+extra1) (
      forall(j in 0..extra2-1) (
        x[i,j+cols] = x[i,j]
      )
   )
   /\
   forall(j in 0..cols-1+extra2)( 
      forall(i in 0..extra1-1) (
        x[i+rows,j] = x[i,j]
      )
   )
;

%
% semi-experimental: 
%   Replace the "y hack" since it restricts the range of dimensions.
%   So this can handle larger problems.
%
%   (On the other hand, using y and alldifferent(y) "drives" the solution
%   faster and don't require long flattening pre-process...)
% 
constraint
  forall(i1,i2 in 0..rows-1, j1,j2 in 0..cols-1 where i1 < i2 \/ j1 < j2) (
      sum(k1 in 0..m1-1, k2 in 0..m2-1) ( 
           bool2int(x[i1+k1, j1+k2] != x[i2+k1, j2+k2])
      ) >= 1
  )
;


%
% Use "y" to be able to use alldifferent(y).
%
% Note: When this is active, it's recommended that the "poor-man's" 
% alldifferent version above is commented out.
%
% For smaller matrices and with small base this is the recommended
% approach.
%
% constraint
%   forall(i in 0..rows-1, j in 0..cols-1) (
%      toNum([ x[i+k1, j+k2] | k1 in 0..m1-1, k2 in 0..m2-1], y[i*cols+j], base)
%   )
%   /\ % now we ensure that the 
%      % converted sub grids are unique
%      % Note: using ":: domain" annotation may require 
%      % _very_ much memory (lots of Gigs) for larger problems. 
%      % So don't.
%   alldifferent(y) % :: domain

%   % symmetry breaking
%   % /\ y[0] = 0
%   % /\ y[0] = sub_max
% ;


% %
% % extra constraints 
% % - global cardinality: the same number of occurrences for all values 0..base-1
%
% constraint
%    % global cardinality count
%    global_cardinality_old([x[i,j] | i in 0..rows-1, j in 0..cols-1], gcc)
%    % global_cardinality([x[i,j] | i in 0..rows-1, j in 0..cols-1], set2array(0..base-1),  gcc)

%    /\ % require that all occurrences of values in x should be the same.
%    forall(i in 1..base-1) (
%       gcc[i-1] = gcc[i]
%    )
% ;

% constraint maximum(max_val, y);

% require that all values > 0
% constraint forall(i,j in 0..dim-1) ( x[i,j] > 0 );

%
% require that all rows/columns (0..dim-1) are distinct
% (and - perhaps - diagonals), i.e. a Latin Square/Sudoku.
%
% int: reg = ceil(sqrt(int2float(rows))); % size of the Sudoku regions
% constraint
%   forall(i in 0..rows-1) ( 
%      alldifferent([x[i,j] | j in 0..cols-1])
%   )
%   /\
%   forall(j in 0..cols-1) (
%      alldifferent([x[i,j] | i in 0..rows-1])
%   )

%   % all subgrids are distinct (very experimental)
%   % /\
%   %   forall(i in 0..rows-1, j in 0..cols-1) (
%   %     alldifferent([ x[i+k1, j+k2] | k1 in 0..m1-1, k2 in 0..m2-1])
%   %   )
  

%   % diagonals and Sudoku sub-grids (if a square)
%   /\
%   if rows = cols then
%     % diagonal
%     alldifferent([x[i,i] | i in 0..rows-1])
%     /\ % diagonal
%     alldifferent([x[i,rows-i] | i in 0..rows-1])  

%     /\
%     if rows = 9 /\ cols = 9 then
%       % Require Sudoku sub grids
%       forall(i in 0..reg-1,j in 0..reg-1) (
%         % trace(show([1+r*cols+c | r in i*reg..(i*reg)+reg-1, c in j*reg..(j*reg)+reg-1]) ++ "\n", 1=1)
%         % /\
%         alldifferent([x[r,c] | r in i*reg..(i*reg)+reg-1, c in j*reg..(j*reg)+reg-1])
%       )
%      else 
%        true
%      endif
%    else 
%      true
%    endif
% ;


%
% A lot of information is printed.
%
output
[
 "x: " ++ show([x[i,j] | i in 0..rows-1, j in 0..cols-1]) ++ "\n"
]
++
["x (rows x cols):" ]
++
[
   if j = 0 then "\n" else " " endif ++
     show(x[i,j])
   | i in 0..rows-1, j in 0..cols-1
] 
++
["\n\nx expanded (rows-1+extra x cols-1+extra):" ]
++
[
   if j = 0 then "\n" else " " endif ++
     show(x[i,j])
   | i in 0..rows-1+extra1, j in 0..cols-1+extra2
] 
++
[
  "\n\nx as " ++ show(m1) ++ " x " ++ show(m2) ++ " m-tuples:\n"
]
++
[
  show([ x[i+k1, j+k2] | k1 in 0..m1-1, k2 in 0..m2-1]) ++  
       " y[" ++ show(i*cols+j) ++ "]: " ++ "\n" % ++ show(y[i*cols+j]) ++  "\n"
  | i in 0..rows-1, j in 0..cols-1
] 
%++
% ["\n\ny: " ++ show(y) ++ "\n"]
% ++
% [
%   if j = 0 then "\n" else " " endif ++
%     if fix(y[i*cols+j]) < 10 then " " else "" endif ++
%       show(y[i*cols+j])
%   | i in 0..rows-1, j in 0..cols-1
% ]
% ++ ["\n\ngcc: " ++ show(gcc) ++ "\n" ]
% ++ ["\n\nmax_val: " ++ show(max_val) ++ "\n" ]
++
["\n"]
;

