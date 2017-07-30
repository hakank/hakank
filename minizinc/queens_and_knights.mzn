% 
% Queens and Knights problem in MiniZinc.
% 
% From Roger K.W. Hui: "Queens and Knights"
% http://archive.vector.org.uk/art10003900
% """
% In 1850, Carl Friedrich Gauss and Franz Nauck showed that it is 
% possible to place eight queens on a chessboard such that no queen 
% attacks any other queen. The problem of enumerating the 92 different 
% ways there are to place 8 queens in this manner has become a standard 
% programming example, and people have shown that it can be solved 
% using many different search techniques.
%
% Now consider a variant of this problem: you must place an 
% equal number of knights and queens on a chessboard such that 
% no piece attacks any other piece. What is the maximum number of 
% pieces you can so place on the board, and how many different 
% ways can you do it?
% """

% 1) The maximum number of pieces is 10: 5 queens and 5 knights
%
% z: 10
% queens: 5
% knights: 5
% 
% _ _ _ _ _ Q _ _
% _ _ Q _ _ _ _ _
% _ _ _ _ _ _ Q _
% _ Q _ _ _ _ _ _
% _ _ _ _ _ _ _ _
% _ _ _ _ _ _ _ Q
% K _ _ K _ _ _ _
% K _ _ K K _ _ _
%
% 2) There are 16 different solutions.
 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 8;

int: queen = 1;
int: knight = 2;

% decision variables
array[1..n,1..n] of var 0..2: x; 

% there can only be n queens so it's the upper limits of knights as well
var 1..n: queens  = sum([x[i,j] = queen  | i,j in 1..n]);
var 1..n: knights = sum([x[i,j] = knight | i,j in 1..n]);
var 2..n*2: z = queens + knights;

% solve maximize z;
% solve satisfy;
% solve :: int_search(array1d(x), input_order, indomain_max, complete) maximize z;
solve :: int_search(array1d(x), input_order, indomain_max, complete) satisfy;

constraint
  forall(i,j in 1..n) (
    if x[i,j] = queen then
        % rows and columns
        forall(a in 1..n where a != i) ( x[a,j] = 0 ) /\
        forall(a in 1..n where a != j) ( x[i,a] = 0 ) /\  
        % diagonals
        forall(a,b in 1..n where a != i /\ b != j) (
          if a+b = i+j \/ a-b = i-j then
             x[a,b] = 0
          else 
             true
          endif
        )
    elseif x[i,j] = knight then
        % knight
        forall(a,b in {-2,-1,1,2} where i+a in 1..n /\ j+b in 1..n /\ abs(a)+abs(b) = 3) (
           x[i+a,j+b] = 0 
        )
    else 
       true
    endif
  )
  /\ queens = knights
  /\ z = 10 % for n=8, optimal number of pieces (i.e. 5+5)
;

output 
[  
  "z: \(z)\nqueens: \(queens)\nknights: \(knights)\n",
]
++
[
  if j = 1 then "\n" else " " endif ++
   if fix(x[i,j]) = 1 then
     "Q"
   elseif fix(x[i,j]) = 2 then
     "K"
   else 
     "_"
   endif 
  | i,j in 1..n
];

