% 
% Strata puzzle in MiniZinc.
% 
% http://appadvice.com/review/quickadvice-strata
% https://en.wikipedia.org/wiki/Strata_%28video_game%29
% http://www.gamingonlinux.com/articles/new-release-strata-a-simple-looking-yet-challenging-puzzle-game-now-on-linux.3798
% """
% The goal of Strata is to strategically layer colored ribbons to match a pattern. Using forethought and 
% strategy you must sequence your moves correctly. Over time patterns emerge and your tactics may evolve, but 
% Strata will keep you on your toes with increasingly difficult puzzles. 
% """

% 
% Note: This model ensures the optimal number of moves.
% 

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: rows;
int: cols;
array[1..rows,1..cols] of int: puzzle;
int: num_colors;
int: max_num_moves = rows+cols;
array[0..num_colors] of string: colors_s;

% decision variables

% the moves:
array[1..max_num_moves] of var 0..num_colors: move_color; % the moves: the color
array[1..max_num_moves] of var 0..rows: move_row;   % the moves: what row (or 0)
array[1..max_num_moves] of var 0..cols: move_column;% the moves: what color (or 0)
array[0..max_num_moves, 1..rows, 1..cols] of var 0..num_colors: y; % the configurations per move
var 1..max_num_moves: num_moves;


% solve satisfy;
solve :: int_search(
        move_color ++ move_row ++ move_column 
        % move_row ++ move_column  ++ move_color
        % ++ 
        % ++[y[m,r,c] | m in 1..max_num_moves, r in 1..rows, c in 1..cols]
        % ++ move_color ++ move_row ++ move_column 
        ,
        first_fail, indomain_max, complete) 
        minimize num_moves;
        % satisfy;

constraint
  % alldifferent_except_0(move_row) /\
  % alldifferent_except_0(move_column) /\

  % init: move 0
  forall(r in 1..rows, c in 1..cols) (
     y[0,r,c] = 0
  ) 
  % /\ move_color[0] = 0
  % /\ move_row[0] = 0
  % /\ move_column[0] = 0

  /\ % moves
  forall(m in 1..max_num_moves) (

     % ((move_row[m] > 0 \/ move_color[m] > 0) <-> move_color[m] > 0) /\

     if move_color[m] > 0 then
        % it's a move
 
        if move_row[m] > 0 then
          % row 
          move_column[m] = 0 /\
          forall(c in 1..cols) (
            y[m,move_row[m],c] = move_color[m]
          )
          /\ % inertia: things that don't change in this move
          forall(r in 1..rows, c in 1..cols) (
            if r != move_row[m] then
              y[m,r,c] = y[m-1,r,c]
            else 
              true
            endif
          )

        else 
          % column
          move_row[m] = 0 /\
          forall(r in 1..rows) (
            y[m,r,move_column[m]] = move_color[m]
          )
          /\ % inertia
          forall(r in 1..rows, c in 1..cols) (
            if c != move_column[m] then
              y[m,r,c] = y[m-1,r,c]
            else 
              true
            endif
          )

        endif
     else 
       % it's not a move (inertia)
       move_row[m] = 0 /\
       move_column[m] = 0 /\
       move_color[m] = 0 /\
       forall(r in 1..rows, c in 1..cols) (
         y[m,r,c] = y[m-1,r,c]
       )
     endif

     /\
     if m > num_moves then
       move_color[m] = 0 /\
       move_row[m] = 0 /\
       move_column[m] = 0
     else 
       true
     endif

  )
  /\ % The goal state
  forall(r in 1..rows, c in 1..cols where puzzle[r,c] > 0) (
     y[num_moves,r,c] = puzzle[r,c]
  ) 

  /\ % redundant but effective
  num_moves = max([m*(move_color[m] > 0)| m in 1..max_num_moves])


  % /\ 
  % % goal
  % % note: exists is converted to var set (which some solve don't support)
  % (exists(m in 1..num_moves) (
  %   forall(r in 1..rows, c in 1..cols where puzzle[r,c] > 0) (
  %     y[m,r,c] = puzzle[r,c]
  %   ) 
  %   /\ num_moves = m 
  % ) 
  % )

  
  % /\ num_moves = 8 % testing for problem 5

;

output 
[
  "move_color : ", show(move_color), "\n",
  "move_row   : ", show(move_row), "\n",
  "move_column: ", show(move_column), "\n",
  "num_moves  : ", show(num_moves), "\n",
  "colors_s   : ", show(colors_s), "\n",
]
++
[
  if c = 1 /\ r = 1 then "\n\n" ++ 
      "* move " ++ show(m) ++ ": " ++ 
      if fix(move_row[m]) > 0 then 
         "row: " ++ show(move_row[m])
      else 
         "column: " ++ show(move_column[m])
      endif 
      ++
      " color " ++ show(move_color[m]) ++ " (" ++ show(colors_s[fix(move_color[m])]) ++ ")\n"
  else  "" 
  endif ++
  if c = 1 then "\n" else "" endif ++
      % show(y[m,r,c]) ++ " "
      show(colors_s[fix(y[m,r,c])]) ++ " "
  | m in 1..max_num_moves, r in 1..rows, c in 1..cols where m <= fix(num_moves)
]
++
[
  "\n\nSummary of moves: \n",
  "num_moves: " ++ show(num_moves), "\n",
]
++
[
  "move " ++ show_int(2, m) ++ ": " ++ 
   if fix(move_row[m]) > 0 then 
     "row    " ++ show(move_row[m])
   else 
     "column " ++ show(move_column[m])
   endif 
   ++
   " " ++ show(colors_s[fix(move_color[m])]) ++ "\n"
 | m in 1..max_num_moves where m <= fix(num_moves)
]
;


%
% data
%

%% Simple 1
% http://www.gamingonlinux.com/articles/new-release-strata-a-simple-looking-yet-challenging-puzzle-game-now-on-linux.3798
% https://www.youtube.com/watch?v=7F6nTxahjNs
% (time 0:03 in the video)

% int: red = 1;
% int: yellow = 2;
% int: green = 3;
% num_colors = 3;
% rows = 2;
% cols = 2;
% colors_s = array1d(0..num_colors, ["-     ",
%                                    "red   ",
%                                    "yellow",
%                                    "green "]);
% puzzle = array2d(1..rows, 1..cols,
%   [
%     red,        0,
%     yellow, green,
%   ]);


% % Example 2
% % http://www.gamingonlinux.com/articles/new-release-strata-a-simple-looking-yet-challenging-puzzle-game-now-on-linux.3798
% % https://www.youtube.com/watch?v=7F6nTxahjNs
% % (time 0:21 in the video)

% int: red = 1;
% int: blue = 2;
% int: green = 3;
% num_colors = 3;
% rows = 3;
% cols = 3;
% colors_s = array1d(0..num_colors, ["-     ",
%                                    "red   ",
%                                    "blue  ",
%                                    "green "]);
% puzzle = array2d(1..rows, 1..cols,
%   [
%      0,   red,   red,
%     red, green,    0,
%     red,     0, blue,
%   ]);


% Example 3
% http://www.gamingonlinux.com/articles/new-release-strata-a-simple-looking-yet-challenging-puzzle-game-now-on-linux.3798
% https://www.youtube.com/watch?v=7F6nTxahjNs
% (time 0:45 in the video)

% int: red = 1;
% int: blue = 2;
% int: green = 3;
% num_colors = 3;
% rows = 3;
% cols = 3;
% colors_s = array1d(0..num_colors, ["-     ",
%                                    "red   ",
%                                    "blue  ",
%                                    "green "]);
% puzzle = array2d(1..rows, 1..cols,
%   [
%     0,     blue, red,
%     green, 0,    green,
%     blue,  0,    red,
%   ]);


%% Another example
%% 
% int: red = 1;
% int: yellow = 2;

% rows = 3;
% cols = 3;
% num_colors = 2;
% colors_s = array1d(0..num_colors, ["-     ", 
%                                    "red   ", 
%                                    "yellow"]);

% puzzle = 
% array2d(1..rows, 1..cols,
% [
%   red,   yellow,   0,
%   red,   yellow, red,
%   yellow,yellow, red,
% ]);


%% From 
%% http://www.gamingonlinux.com/articles/new-release-strata-a-simple-looking-yet-challenging-puzzle-game-now-on-linux.3798
%% (time 1:04 in the video)
%%
%%    green, 0,     red,      red,    red,
%%    green, 0,     orange, green, orange,
%%    0    , green, red,      red,    red,
%%    green, green, brown,  brown,  brown,
%%    0    , green, green,      0,  green, 
%%
%% 
%% Summary of moves: 
%% move 1: row    2 orange
%% move 2: column 4 green 
%% move 3: row    5 green 
%% move 4: row    4 brown 
%% move 5: row    3 red   
%% move 6: column 2 green 
%% move 7: row    1 red   
%% move 8: column 1 green 
%%

rows = 5;
cols = 5;
num_colors = 4;

int: green = 1;
int: red = 2;
int: orange = 3;
int: brown = 4;

colors_s = array1d(0..num_colors, ["-     ", 
                                   "green ", 
                                   "red   ", 
                                   "orange", 
                                   "brown "]);

puzzle = 
array2d(1..rows, 1..cols,
[
   green, 0,     red,      red,    red,
   green, 0,     orange, green, orange,
   0    , green, red,      red,    red,
   green, green, brown,  brown,  brown,
   0    , green, green,      0,  green, 
]);
