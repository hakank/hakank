% 
% Nonogram (a.k.a. Painting by Numbers) in MiniZinc.
% 
% http://en.wikipedia.org/wiki/Nonogram
% """
% Nonograms or Paint by Numbers are picture logic puzzles in which cells in a 
% grid have to be colored or left blank according to numbers given at the 
% side of the grid to reveal a hidden picture. In this puzzle type, the 
% numbers measure how many unbroken lines of filled-in squares there are 
% in any given row or column. For example, a clue of "4 8 3" would mean 
% there are sets of four, eight, and three filled squares, in that order, 
% with at least one blank square between successive groups.
%
% """

%  See problem 12 at http://www.csplib.org/.
% 
% http://www.puzzlemuseum.com/nonogram.htm
%
% Haskell solution:
% http://twan.home.fmf.nl/blog/haskell/Nonograms.details


% Brunetti, Sara & Daurat, Alain (2003)
% "An algorithm reconstructing convex lattice sets"
% http://geodisi.u-strasbg.fr/~daurat/papiers/tomoqconv.pdf


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 

int: rows;
int: cols;

int: row_rule_len; % max length of row rules
array[1..rows, 1..row_rule_len] of int: row_rules;

int: col_rule_len; % max length of col rules
array[1..cols, 1..col_rule_len] of int: col_rules;

% variable
array[1..rows, 1..cols] of var 0..1: board;

% I tried a version where start_pos was a let variable in check_rule.
% These global variable seems to make it more efficient.
array[1..rows, 1..row_rule_len] of var 0..cols: row_start_pos;
array[1..cols, 1..col_rule_len] of var 0..rows: col_start_pos;

%
% a helper predicate (cf alldifferent_except_0.mzn)
%
predicate all_different_except_0(array[int] of var int: x) =
   let {
      int: n = length(x)
   }
   in
   forall(i,j in 1..n where i != j) (
        (x[i] > 0 /\ x[j] > 0) -> x[i] != x[j]
   )
;

%
% check_rule 
%
% Check a row/column against the rule.
%
predicate check_rule(array[int] of int: rules, array[int] of var int: y, array[int] of var int: start_pos) =
   let {
      int: n = length(y),
      int: r_len = length(rules)
   }
   in
   forall(i in 1..r_len where rules[i] = 0) (    
     rules[i] = 0 <-> start_pos[i] = 0
   )
   /\
   forall(i in 1..r_len where rules[i] > 0) (
      exists(j, k in 1..n where k = j + rules[i]-1) (

         start_pos[i] = j

         /\ % fill the range
         forall(m in j..k) (
            y[m] = 1 
         )

         /\ % ordering of start_pos
         if i > 1 /\ rules[i] > 0 then 
            start_pos[i] > start_pos[i-1] + rules[i-1]
         else 
            true
         endif

         /\ % must be a blank between the patterns 
            % (not before/after the extremal positions, though)
         if j > 1 then y[j-1] = 0 else  true  endif
         /\
         if k < n then y[k+1] = 0 else  true  endif
      ) 
   )

  /\
  sum(rules) = sum(y)

  /\
  % Efficiency: the more constraints the merrier. :-)
  all_different_except_0(start_pos)
%  /\ 
%  increasing(start_pos)
;


% solve satisfy;
solve :: int_search([board[i,j] | i in 1..rows, j in 1..cols] ++ [row_start_pos[i,j] | i in 1..rows, j in 1..row_rule_len] ++ [col_start_pos[i,j] | i in 1..cols, j in 1..col_rule_len], first_fail, indomain_split, complete) satisfy;
% solve :: int_search([board[i,j] | i in 1..rows, j in 1..cols] , first_fail, indomain_split, complete) satisfy;



constraint

    forall(i in 1..rows) (
       check_rule([row_rules[i,j] | j in 1..row_rule_len], [board[i,j] | j in 1..cols], [row_start_pos[i, j] | j in 1..row_rule_len])
    )
    /\
    forall(j in 1..cols) (
       check_rule([col_rules[j,k] | k in 1..col_rule_len], [board[i, j] | i in 1..rows], [col_start_pos[j, k] | k in 1..col_rule_len])
    )
;

output [
  if j = 1 then "\n" else "" endif ++
    % show_cond(board[i,j] > 0, "#", " ")  % for minizinc and ic
    show(board[i,j])  % for fz and fzntini
  | i in 1..rows, j in 1..cols
] ++ ["\n"];


%
% data
%

%% simple test
%%   _ X _ _
%%   X _ X X
%%   _ X X _
%%   X _ X X
%%   X _ _ _
%% 
% rows = 5;
% row_rule_len = 2;
% row_rules = array2d(1..rows, 1..row_rule_len, 
%     [
%       0,1,
%       1,2,
%       0,2,
%       1,2,
%       0,1
%     ]);
%
% cols = 4;
% col_rule_len = 2;
% col_rules = array2d(1..cols, 1..col_rule_len,
%     [
%       1,2,
%       1,1,
%       0,3,
%       1,1,
%     ]);

%%
%% Simple variant:
%% from http://twan.home.fmf.nl/blog/haskell/Nonograms.details
%% (2 solutions)
% rows = 4;
% row_rule_len = 2;
% row_rules = array2d(1..rows, 1..row_rule_len, 
%     [
%       1,1,
%       1,1,
%       1,1,
%       1,1
%     ]);

% cols = 4;
% col_rule_len = 2;
% col_rules = array2d(1..cols, 1..col_rule_len,
%     [
%       1,1,
%       1,1,
%       1,1,
%       1,1,
%     ]);



%% From http://twan.home.fmf.nl/blog/haskell/Nonograms.details
%% The lambda picture
%% 
%% fzntini: 3.5 seconds.
%% fz: ??
rows = 12;
row_rule_len = 3;
row_rules = array2d(1..rows, 1..row_rule_len, 
        [0,0,2,
         0,1,2,
         0,1,1,
         0,0,2,
         0,0,1,
         0,0,3,
         0,0,3,
         0,2,2,
         0,2,1,
         2,2,1,
         0,2,3,
         0,2,2
     ]);

cols = 10;
col_rule_len = 2;
col_rules = array2d(1..cols, 1..col_rule_len,
         [2,1,
          1,3,
          2,4,
          3,4,
          0,4,
          0,3,
          0,3,
          0,3,
          0,2,
          0,2
         ]);


%% 
%% ECLiPSe 
%% http://eclipse.crosscoreop.com/eclipse/examples/nono.ecl.txt
%% Problem ps
% rows = 9;
% row_rule_len = 2;
% row_rules = array2d(1..rows, 1..row_rule_len,
%         [0,3,
%          2,1,
%          3,2,
%          2,2,
%          0,6,
%          1,5,
%          0,6,
%          0,1,
%          0,2]);
%
% cols = 8;
% col_rule_len = 2;
% col_rules = array2d(1..cols, 1..col_rule_len,
%      [1,2,
%       3,1,
%       1,5,
%       7,1,
%       0,5,
%       0,3,
%       0,4,
%       0,3		
%   ]);


%% 
%% ECLiPSe
%% http://eclipse.crosscoreop.com/eclipse/examples/nono.ecl.txt
%% Problem n2 ( http://www.pro.or.jp/~fuji/java/puzzle/nonogram/index-eng.html )
%%
%% fztini 4.5 seconds
%% fz: with int_search on row_start_pos ++  col_start_pos: the solution comes after
%%     3 seconds and then 7 minutes for verifying that it is the only solution.
%%     with int_search on row_start_pos ++  col_start_pos and -solutions 1: 4 sec!
% rows = 10;
% row_rule_len = 4;
% row_rules = array2d(1..rows, 1..row_rule_len,
%     [
%      0,0,0,1,
%      0,0,0,3,
%      0,0,1,3,
%      0,0,2,4,
%      0,0,1,2,
%      0,2,1,1,
%      1,1,1,1,
%      0,2,1,1,
%      0,0,2,2,
%      0,0,0,5
%         ]);

% cols = 10;
% col_rule_len = 4;
% col_rules = array2d(1..cols, 1..col_rule_len,
%     [0,0,0,4,
%      0,0,1,3,
%      0,0,2,3,
%      0,0,1,2,
%      0,0,2,2,
%      0,1,1,1,
%      1,1,1,1,
%      0,1,1,1,
%      0,0,1,2,
%      0,0,0,5]);


%% ECLiPSe
%% http://eclipse.crosscoreop.com/eclipse/examples/nono.ecl.txt
%% Problem n3 ( http://www.pro.or.jp/~fuji/java/puzzle/nonogram/index-eng.html )
%%
%% fztini: 6.5 sec
%% fz: ??
% rows = 10;
% row_rule_len = 4;
% row_rules = array2d(1..rows, 1..row_rule_len,
%    [
%    0,0,0,4,
%    0,1,1,6,
%    0,1,1,6,
%    0,1,1,6,
%    0,0,4,9,
%    0,0,1,1,
%    0,0,1,1,
%    0,2,7,2,
%    1,1,1,1,
%    0,0,2,2
%    ]);

% cols = 15;
% col_rule_len = 2;
% col_rules = array2d(1..cols, 1..col_rule_len,
%    [
%    0,4,
%    1,2,
%    1,1,
%    5,1,
%    1,2,
%    1,1,
%    5,1,
%    1,1,
%    4,1,
%    4,1,
%    4,2,
%    4,1,
%    4,1,
%    4,2,
%    0,4
%    ]);


%% http://eclipse.crosscoreop.com/eclipse/examples/nono.ecl.txt
%% Problem n4
%% fz: < 1 sec
% rows = 6;
% row_rule_len = 2;
% row_rules = array2d(1..rows, 1..row_rule_len,
%    [
%     2,1,
%     0,1,
%     0,2,
%     0,2,
%     0,1,
%     1,2,
%    ]);

% cols = 6;
% col_rule_len = 2;
% col_rules = array2d(1..cols, 1..col_rule_len,
%    [
%     1,2,
%     0,1,
%     0,2,
%     0,2,
%     0,1,
%     2,1
%    ]);

%% http://eclipse.crosscoreop.com/eclipse/examples/nono.ecl.txt
%% % Problem n5
%% fz: < 2 sec
% rows = 10;
% row_rule_len = 2;
% row_rules = array2d(1..rows, 1..row_rule_len,
%    [
%     0,3,
%     0,3,
%     0,1,
%     0,3,
%     0,6,
%     0,3, 
%     0,3,
%     3,3,
%     2,2,
%     2,1,
%    ]);

% cols = 10;
% col_rule_len = 2;
% col_rules = array2d(1..cols, 1..col_rule_len,
%    [
%     0,1,
%     1,2,
%     1,2,
%     1,1,
%     2,5,
%     0,7,
%     2,5,
%     0,1,
%     0,2,
%     0,2
%    ]);


%% http://eclipse.crosscoreop.com/eclipse/examples/nono.ecl.txt
%% Problem n6
%% fztini 18 sec
%% 
% rows = 15;
% row_rule_len = 4;
% row_rules = array2d(1..rows, 1..row_rule_len,
%  [0,0,0,5,
%   0,0,2,2,
%   0,0,1,1,
%   0,0,1,1,
%   0,0,4,4,
%   2,2,1,2,
%   0,1,3,1,
%   1,1,1,1,
%   0,2,7,2,
%   0,4,1,5,
%   0,2,1,1,
%   0,1,1,2,
%   0,1,1,1,
%   0,2,5,2,
%   0,0,3,4]);

% cols = 15;
% col_rule_len = 4;
% col_rules = array2d(1..cols, 1..col_rule_len,
%  [0,0,0,4,
%   0,0,2,2,
%   0,0,1,5,
%   0,1,2,2,
%   0,5,2,1,
%   2,1,1,2,
%   0,1,3,1,
%   0,1,1,6,
%   0,1,3,1,
%   2,1,2,2,
%   0,4,2,1,
%   0,1,1,1,
%   0,1,3,2,
%   0,2,2,3,
%   0,0,0,4]);




%% From Wikipedia
%% http://en.wikipedia.org/wiki/Nonogram
%% More details:
%% http://en.wikipedia.org/wiki/Image:Paint_by_numbers_Animation.gif
%% fztini: 7:48 minutes
%% fz: ??
% rows = 20;
% row_rule_len = 5;
% row_rules = array2d(1..rows, 1..row_rule_len,
%     [
%          0,0,0,0,3,
%          0,0,0,0,5,
%          0,0,0,3,1,
%          0,0,0,2,1,
%          0,0,3,3,4,

%          0,0,2,2,7,
%          0,0,6,1,1,
%          0,0,4,2,2,
%          0,0,0,1,1,
%          0,0,0,3,1,

%          0,0,0,0,6,
%          0,0,0,2,7,
%          0,0,6,3,1,
%          1,2,2,1,1,
%          0,4,1,1,3,

%          0,0,4,2,2,
%          0,0,3,3,1,
%          0,0,0,3,3,
%          0,0,0,0,3,
%          0,0,0,2,1,         
%     ]);

% cols = 20;
% col_rule_len = 5;
% col_rules = array2d(1..cols, 1..col_rule_len,
%     [
%          0,0,0,0,2,
%          0,0,0,1,2,
%          0,0,0,2,3,
%          0,0,0,2,3,
%          0,0,3,1,1,

%          0,0,2,1,1,
%          1,1,1,2,2,
%          1,1,3,1,3,
%          0,0,2,6,4,
%          0,3,3,9,1,

%          0,0,5,3,2,
%          0,3,1,2,2,
%          0,0,2,1,7,
%          0,0,3,3,2,
%          0,0,0,2,4,

%          0,0,2,1,2,
%          0,0,2,2,1,
%          0,0,0,2,2,
%          0,0,0,0,1,
%          0,0,0,0,1,
%     ]); 


% https://prof.ti.bfh.ch/hew1/informatik3/prolog/p-99/p98.pl
% 'Hen'
% fztini: 0.5 sec
% Gecode/fz: 0.7 sec
% MiniZinc: 1 sec
% rows = 9;
% row_rule_len = 2;
% row_rules = array2d(1..rows, 1..row_rule_len,
%     [0,3, 
%     2,1, 
%     3,2, 
%     2,2, 
%     0,6, 
%     1,5, 
%     0,6, 
%     0,1, 
%     0,2]);

% cols = 8;
% col_rule_len = 2;
% col_rules = array2d(1..cols, 1..col_rule_len,
%     [1,2,
%      3,1, 
%      1,5, 
%      7,1, 
%      0,5, 
%      0,3, 
%      0,4, 
%      0,3]);


% http://www.ida.liu.se/~TDDB80/lisp/projekt/Projektkatalog/Nonogram/
%%
%% Gecode/fz: 2 sec
%% flatzinc: 1.6
%% fztini: 2 sec
% rows = 10;
% row_rule_len = 3;
% row_rules = array2d(1..rows, 1..row_rule_len,
%        [
%          0,0,4,
%          0,0,6,
%          2,2,2,
%          2,2,2,
%          0,0,10,
%          0,0,10,
%          2,2,2,
%          0,3,3,
%          0,0,6,
%          0,0,4]);

% cols = 10;
% col_rule_len = 3;
% col_rules = array2d(1..cols, 1..col_rule_len,
%     [
%      0,0,4,
%      0,0,6,
%      2,2,2,
%      2,2,3,
%      0,7,2,
%      0,7,2,
%      2,2,3,
%      2,2,2,
%      0,0,6,
%      0,0,4
%     ]);



%% http://www.puzzlemuseum.com/griddler/griddler.htm
%% (as Griddlers)
%% ECLiPSe/ic: 2.3 sec
%% fztini: 1.1 sec
%% minizinc: 1.4 sec
%% Gecode/fz: 1 sec 
% rows = 10;
% row_rule_len = 3;
% row_rules = array2d(1..rows, 1..row_rule_len,
%         [
%          0,0,5,
%          1,1,1,
%          0,0,5,
%          0,0,1,
%          0,0,5,
%          1,1,2,
%          0,0,1,
%          0,0,5,
%          0,1,1,
%          0,2,2,
%         ]);

% cols = 7;
% col_rule_len = 4;
% col_rules = array2d(1..cols, 1..col_rule_len,
%      [
%        0,0,0,1,
%        0,3,2,3,
%        1,1,1,1,
%        0,0,0,8,
%        1,1,1,1,
%        0,3,2,3,
%        0,0,1,1,
%      ]);
