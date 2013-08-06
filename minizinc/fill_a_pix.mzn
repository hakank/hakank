% 
% Fill-a-Pix problem in MiniZinc.
% 
% From http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/basiclogic
% """
% Each puzzle consists of a grid containing clues in various places. The 
% object is to reveal a hidden picture by painting the squares around each 
% clue so that the number of painted squares, including the square with 
% the clue, matches the value of the clue. 
% """
%
% Other names of this puzzle:
%
%     * ぬり絵パズル
%     * Nurie-Puzzle
%     * Majipiku
%     * Oekaki-Pix
%     * Mosaic
%     * Mosaik
%     * Mozaïek
%     * ArtMosaico
%     * Count and Darken
%     * Nampre puzzle
%     * Komsu Karala!
%     * Cuenta Y Sombrea
%     * Mosaico
%     * Voisimage
%     * Magipic
%     * Fill-In

%
% http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/rules
% """
% Fill-a-Pix is a Minesweeper-like puzzle based on a grid with a pixilated 
% picture hidden inside. Using logic alone, the solver determines which 
% squares are painted and which should remain empty until the hidden picture 
% is completely exposed.
% """
% 
% Fill-a-pix History:
% http://www.conceptispuzzles.com/index.aspx?uri=puzzle/fill-a-pix/history


%
% Also, see the Minesweeper model
% http://www.hakank.org/minizinc/minesweeper.mzn
%

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: n;
array[1..n, 1..n] of -1..9: puzzle;
array[1..n, 1..n] of var 0..1: x;
int: X = -1; % blank

solve satisfy;
% solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
    % This is from
    % http://www.hakank.org/minizinc/minesweeper.mzn
    forall(i,j in 1..n) (
      (
       (puzzle[i,j] >= 0 )
        ->
        puzzle[i,j]  = sum(a,b in {-1,0,1} where 
            i+a > 0  /\ j+b >  0 /\
            i+a <= n /\ j+b <= n
         )
           (x[i+a,j+b])
      )
    )
;


output [
   if j = 1 then ",\n" else "," endif ++
     show(x[i,j])
   | i, j in 1..n

]
++
["\n"]
;