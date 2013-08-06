%
% Minesweeper "inverse problem" in MiniZinc.
%
% This is the inverse of the minesweeper problem:
%    Given the positions of the boms, calculate
%    the number of neighbours.

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: r; % rows
int: c; % column
int: X = -1; % the unknowns

% encoding: -1 for unknown, >= 0 for number of mines in the neighbourhood
% array[1..r, 1..c] of -1..8: game;
% array[1..r, 1..c] of var 0..1: mines;

array[1..r, 1..c] of var -1..8: game;
array[1..r, 1..c] of  0..1: mines;


constraint
    forall(i in 1..r, j in 1..c) (
        game[i,j]  = sum(a,b in {-1,0,1} where 
            i+a > 0  /\ j+b >  0 /\
            i+a <= r /\ j+b <= c
         )
           (mines[i+a,j+b])
      % )
      % /\
      % (game[i,j] > X -> mines[i,j] = 0) 
      %/\
      % (game[i,j] = X <- mines[i,j] = 1) 
    )
;

% solve satisfy;
solve :: int_search([game[i,j] | i in 1..r, j in 1..c], first_fail, indomain_min, complete) satisfy;

%
% data
%

% The first 10 examples are from Gecode/examples/minesweeper.cc
% http://www.gecode.org/gecode-doc-latest/minesweeper_8cc-source.html
% """
% The instances are taken from
%   http://www.janko.at/Raetsel/Minesweeper/index.htm
% """

% Problem from Gecode/examples/minesweeper.cc  problem 0
% r = 6;
% c = 6;
% game = array2d(1..r, 1..c, [
%     X,X,2,X,3,X,
%     2,X,X,X,X,X,
%     X,X,2,4,X,3,
%     1,X,3,4,X,X,
%     X,X,X,X,X,3,
%     X,3,X,3,X,X,
%     ]);


% Problem from Gecode/examples/minesweeper.cc  problem 1
% r = 8;
% c = 8;
% game = array2d(1..r, 1..c, [
%    X,2,X,2,1,1,X,X,
%    X,X,4,X,2,X,X,2,
%    2,X,X,2,X,X,3,X,
%    2,X,2,2,X,3,X,3,
%    X,X,1,X,X,X,4,X,
%    1,X,X,X,2,X,X,3,
%    X,2,X,2,2,X,3,X,
%    1,X,1,X,X,1,X,1,
% ]);


% Problem from Gecode/examples/minesweeper.cc  problem 2
r = 10;
c = 10;
% game = array2d(1..r, 1..c, [
%       1,X,X,2,X,2,X,2,X,X,
%       X,3,2,X,X,X,4,X,X,1,
%       X,X,X,1,3,X,X,X,4,X,
%       3,X,1,X,X,X,3,X,X,X,
%       X,2,1,X,1,X,X,3,X,2,
%       X,3,X,2,X,X,2,X,1,X,
%       2,X,X,3,2,X,X,2,X,X,
%       X,3,X,X,X,3,2,X,X,3,
%       X,X,3,X,3,3,X,X,X,X,
%       X,2,X,2,X,X,X,2,2,X
%       ]);

mines = array2d(1..r, 1..c, [
0,0,0,0,1,0,0,0,0,0,
1,0,0,1,0,1,0,1,1,0,
1,1,0,0,0,1,0,1,0,0,
0,0,0,0,0,0,0,0,1,0,
1,0,0,0,0,0,1,0,1,0,
0,0,1,0,1,0,0,0,0,0,
0,1,0,0,0,0,1,0,0,0,
1,0,0,0,1,0,0,0,1,0,
0,1,0,1,0,0,1,0,1,1,
0,0,1,0,0,1,0,0,0,0 
]);


% Problem from Gecode/examples/minesweeper.cc  problem 3
% r = 8;
% c = 8;
% game = array2d(1..r,1..c,[
%       2,X,X,X,3,X,1,X,
%       X,5,X,4,X,X,X,1,
%       X,X,5,X,X,4,X,X,
%       2,X,X,X,4,X,5,X,
%       X,2,X,4,X,X,X,2,
%       X,X,5,X,X,4,X,X,
%       2,X,X,X,5,X,4,X,
%       X,3,X,3,X,X,X,2,
% ]);


% Problem from Gecode/examples/minesweeper.cc  problem 4
% r = 10;
% c = 10;
% game = array2d(1..r,1..c,[
%       0,X,0,X,1,X,X,1,1,X,
%       1,X,2,X,2,X,2,2,X,X,
%       X,X,X,X,X,X,2,X,X,2,
%       X,2,3,X,1,1,X,X,X,X,
%       0,X,X,X,X,X,X,2,X,1,
%       X,X,X,2,2,X,1,X,X,X,
%       X,X,X,X,X,3,X,3,2,X,
%       X,5,X,2,X,X,X,3,X,1,
%       X,3,X,1,X,X,3,X,X,X,
%       X,2,X,X,X,1,2,X,X,0,
% ]);


% Problem from Gecode/examples/minesweeper.cc  problem 5
% r = 10;
% c = 10;
% game = array2d(1..r,1..c,[
%       X,2,1,X,2,X,2,X,X,X,
%       X,4,X,X,3,X,X,X,5,3,
%       X,X,X,4,X,4,4,X,X,3,
%       4,X,4,X,X,5,X,6,X,X,
%       X,X,4,5,X,X,X,X,5,4,
%       3,4,X,X,X,X,5,5,X,X,
%       X,X,4,X,4,X,X,5,X,5,
%       2,X,X,3,3,X,6,X,X,X,
%       3,6,X,X,X,3,X,X,4,X,
%       X,X,X,4,X,2,X,2,1,X,
% ]);


% Problem from Gecode/examples/minesweeper.cc  problem 6
% r = 8;
% c = 8;
% game = array2d(1..r,1..c,[
%        X,3,2,X,X,1,X,X,
%        X,X,X,X,1,X,X,3,
%        3,X,X,2,X,X,X,4,
%        X,5,X,X,X,5,X,X,
%        X,X,6,X,X,X,5,X,
%        3,X,X,X,5,X,X,4,
%        2,X,X,5,X,X,X,X,
%        X,X,2,X,X,3,4,X,
% ]);


% Problem from Gecode/examples/minesweeper.cc  problem 7
% r = 9;
% c = 9;
% game = array2d(1..r,1..c,[
%        X,1,X,X,X,X,X,3,X,
%        X,X,X,3,4,3,X,X,X,
%        2,4,4,X,X,X,4,4,3,
%        X,X,X,4,X,4,X,X,X,
%        X,4,X,4,X,3,X,6,X,
%        X,X,X,4,X,3,X,X,X,
%        1,2,3,X,X,X,1,3,3,
%        X,X,X,3,2,2,X,X,X,
%        X,2,X,X,X,X,X,3,X,
% ]);


% Problem from Gecode/examples/minesweeper.cc  problem 8
% r=7;
% c=7;
% game = array2d(1..r,1..c,[
%        X,X,X,X,X,X,X,
%        X,2,3,4,3,5,X,
%        X,1,X,X,X,3,X,
%        X,X,X,5,X,X,X,
%        X,1,X,X,X,3,X,
%        X,1,2,2,3,4,X,
%        X,X,X,X,X,X,X,
% ]);

% Problem from Gecode/examples/minesweeper.cc  problem 9
%       2...2...2
%       .4.4.3.4.
%       ..4...1..
%       .4.3.3.4.
%       2.......2
%       .5.4.5.4.
%       ..3...3..
%       .4.3.5.6.
%       2...1...2
% r = 9;
% c = 9;
% game = array2d(1..r,1..c,[
%        2,X,X,X,2,X,X,X,2,
%        X,4,X,4,X,3,X,4,X,
%        X,X,4,X,X,X,1,X,X,
%        X,4,X,3,X,3,X,4,X,
%        2,X,X,X,X,X,X,X,2,
%        X,5,X,4,X,5,X,4,X,
%        X,X,3,X,X,X,3,X,X,
%        X,4,X,3,X,5,X,6,X,
%        2,X,X,X,1,X,X,X,2,
% ]);


% From "Some Minesweeper Configurations",page 2
% r = 6;
% c = 6;
% game = array2d(1..r,1..c,[
%      X,X,X,X,X,X,
%      X,2,2,2,2,X,
%      X,2,0,0,2,X,
%      X,2,0,0,2,X,
%      X,2,2,2,2,X,
%      X,X,X,X,X,X,
% ]);


% From "Some Minesweeper Configurations",page 3
% 4 solutions
% r=8;
% c=8;
% game = array2d(1..r,1..c,[
%    2,3,X,2,2,X,2,1,
%    X,X,4,X,X,4,X,2,
%    X,X,X,X,X,X,4,X,
%    X,5,X,6,X,X,X,2,
%    2,X,X,X,5,5,X,2,
%    1,3,4,X,X,X,4,X,
%    0,1,X,4,X,X,X,3,
%    0,1,2,X,2,3,X,2 
% ]);


% Richard Kaye: How Complicated is Minesweeper?
% http://web.mat.bham.ac.uk/R.W.Kaye/minesw/ASE2003.pdf
%
% A Wire,page 33
% (2 solutions)
% r = 5;
% c = 14;
% game = array2d(1..r,1..c,[
%     X,0,0,0,0,0,0,0,0,0,0,0,0,X,
%     X,1,1,1,1,1,1,1,1,1,1,1,1,X,
%     X,X,1,X,X,1,X,X,1,X,X,1,X,X,
%     X,1,1,1,1,1,1,1,1,1,1,1,1,X,
%     X,0,0,0,0,0,0,0,0,0,0,0,0,X,
% ]);

% Richard Kaye: How Complicated is Minesweeper?
% http://web.mat.bham.ac.uk/R.W.Kaye/minesw/ASE2003.pdf
% A splitter,page 35
% A lot of solutions...
% r=11;
% c=11;
% game = array2d(1..r,1..c,[
%   X,X,X,0,X,X,X,0,X,X,X,
%   X,X,X,0,1,X,1,0,X,X,X,
%   X,X,X,0,1,X,1,0,X,X,X,
%   0,0,0,0,1,1,1,0,0,0,0,
%   X,1,1,1,1,X,1,1,1,1,X,
%   X,X,X,1,X,2,X,1,X,X,X,
%   X,1,1,1,1,X,1,1,1,1,X,
%   0,0,0,0,1,1,1,0,0,0,0,
%   X,X,X,0,1,X,1,0,X,X,X,
%   X,X,X,0,1,X,1,0,X,X,X,
%   X,X,X,0,X,X,X,0,X,X,X,
% ]);


% 
% Oleg German, Evgeny Lakshtanov: "Minesweeper" without a computer
% http://arxiv.org/abs/0806.3480, page 4
% r = 5;
% c = 6;
% game = array2d(1..r, 1..c, [
%     X,1,X,1,X,1,
%     2,X,2,X,1,X,
%     X,3,X,2,X,1,
%     1,X,3,X,2,X,
%     X,1,X,2,X,1,
% ]);

% The bombs:
% mines = array2d(1..r, 1..c, [
% 0,0,0,0,1,0, 
% 0,1,0,0,0,0, 
% 1,0,1,0,0,0, 
% 0,0,0,1,0,1, 
% 0,0,1,0,0,0, 
% ]);


output 
[ "\nmines:" ] ++
[
  if j = 1 then "\n" else " " endif ++
    show(mines[i,j])
  | i in 1..r,j in 1..c
] ++
["\ngame:"] ++
[
  if j = 1 then "\n" else " " endif ++
    if mines[i,j] = 1 then
       show("B") 
    else 
      show(game[i,j])
    endif
  | i in 1..r ,j in 1..c
] ++ ["\n"]
;

