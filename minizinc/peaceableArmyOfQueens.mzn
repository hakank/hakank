%
% Peacable Army of queens in MiniZinc.
%
% Translation of the ESSENCE' model in the Minion Translator examples:
% http://www.cs.st-andrews.ac.uk/~andrea/examples/peacableArmyOfQueens/peaceableArmyOfQueens.eprime
% """
% Place 2 equally-sized armies of queens (white and black)
% on a chess board without attacking each other
% Maximise the size of the armies.
%
% 'occurrence' representation 
% """
%
% Also see:
% "Peaceable Queens - Numberphile"
% (Neil Sloane discusses peaceable queens and chess)
% https://www.youtube.com/watch?v=IN1fPtY9jYg
% 

%
% There are 18 solutions for n=5
% For n=6: 560 solutions

%
% Max number of (white) queens for specific N
%
% N   # max queens   PicatSAT
% ---------------------------
% 1   0               0.01s
% 2   0               0.04s
% 3   1               0.06s
% 4   2               0.07s
% 5   4               0.74s
% 6   5               0.60s
% 7   7               3.6s
% 8   9              12.8s 
% 9  12              37.3s 
% 10 14              84.9s
% 11 17             950.59s
% 12
% 13
% 14
% 15
% 
%
% Cf: https://oeis.org/A250000
% "Peaceable coexisting armies of queens: the maximum number m such that m white queens
%  and m black queens can coexist on an n X n chessboard without attacking each other."
% N: 1  2  3  4  5  6  7  8   9  10  11  12  13  14  15
% m: 0  0  1  2  4  5  7  9  12  14  17  21  24  28  32 
%

% 
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
% 

% board width
int: n;
set of int: N = 1..n;


% 0: empty field, 1:white queen, 2: black queen
array[N,N] of var 0..2: board;
var 1..(n*n) div 2: amountOfQueens;


% solve maximize amountOfQueens;        
% solve :: int_search([board[i,j] | i,j in N], first_fail, indomain_min, complete) maximize amountOfQueens;
solve :: int_search([board[i,j] | i,j in N], anti_first_fail, indomain_min, complete) maximize amountOfQueens;
% solve :: int_search([board[i,j] | i,j in N], first_fail, indomain_min, complete) satisfy;

% opposite color: 1 <-> 2
function int: opposite(int: q) = if q = 1 then 2 else 1 endif;


constraint
   board[1,1] = 1  % let's place a white queen in the north-west corner    
   /\
   % we have the same amount of white as black queens
   sum(row, col in N) ( bool2int(board[row,col] = 1)) = amountOfQueens

   /\
   sum(row, col in N) ( bool2int(board[row,col] = 2) ) = amountOfQueens

  /\

  % if we have a white queen at position row and column
  % there is no field on the same row/column/diagonal
  % that holds a black queen
  % 
  % if we have a black queen at position row and column
  % there is no field on the same row/column/diagonal
  % that holds a white queen
  % 

  forall(row,col in N) (
    forall(q in 1..2) (      
      if board[row, col] = q then
        forall(i in N) (
          if i != row then board[i,col] != opposite(q) endif /\
          if i != col then board[row,i] != opposite(q) endif /\ 
          if row+i <= n /\ col+i <= n then board[row+i,col+i] != opposite(q) endif /\ 
          if row-i > 0  /\ col-i > 0  then board[row-i,col-i] != opposite(q) endif /\ 
          if row+i <= n /\ col-i > 0  then board[row+i,col-i] != opposite(q) endif /\ 
          if row-i > 0  /\ col+i <= n then board[row-i,col+i] != opposite(q) endif
       )
     endif
    )
 )
;

output 
[
  "amountOfQueens: ", show(amountOfQueens)
]
++
[
  if j = 1 then "\n" else " " endif ++
    % show(board[i,j])
    if fix(board[i,j]) == 1 then "w" elseif fix(board[i,j]) == 2 then "b" else "_" endif
  | i,j in N
] ++ ["\n"];


%
% data 
%
% n = 1;
% n = 2;
% n = 3;
% n = 4;
% n = 5;
n = 6;
% n = 7;
% n = 8;
% n = 9;
% n = 10; 
% n = 11;
% n = 12;
% n = 13:
% n = 14;
% n = 15;
