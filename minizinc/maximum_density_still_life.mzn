% 
% Maximum density still life in MiniZinc.
% 
% 
% CSPLib 032: http://www.csplib.org/prob/prob032
%
%
% This model was inspired by the OPL model from
% Toni Mancini, Davide Micaletto, Fabio Patrizi, Marco Cadoli: 
% "Evaluating ASP and commercial solvers on the CSPLib"
% http://www.dis.uniroma1.it/~tmancini/index.php?problemid=032&solver=OPL&spec=BASE&currItem=research.publications.webappendices.csplib2x.problemDetails#listing
%
%
% Compare with the Comet model:
% http://www.hakank.org/comet/maximum_density_still_life_cp.co
%
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

% include "globals.mzn"; 
int: size = 7; % to change

set of int: objFunctionBoardCoord      = 2..size+1;
set of int: checkConstraintsBoardCoord = 1..size+2;
set of int: augmentedBoardCoord        = 0..size+3;

% Search space: The set of all possible assignments of 0s (dead) and 1s (live) 
% to the cells of the board section. However, to be able to easily express 
% constraints on "boundary" cells, we take as search space the set of 0/1 
% boards of size n+4 by n+4: the actual stable pattern appears in the sub-board 
% defined by ignoring the first/last two rows/columns.
array[augmentedBoardCoord,augmentedBoardCoord] of var 0..1: grid;

var int: z = sum(r in objFunctionBoardCoord, c in objFunctionBoardCoord) (grid[r,c]);

% Objective function: Maximize the number of live cells in the sub-board defined 
% by ignoring the first/last two/ rows/columns.


% solve maximize z;
solve :: int_search(
        [grid[i,j] | i,j in augmentedBoardCoord], 
        smallest, 
        indomain_max, 
        complete) 
    maximize z;

constraint

  % C1: Cells in the first/last two rows/columns are all 0 (dead)
  forall(x in augmentedBoardCoord) (
    grid[0,x] = 0 /\
    grid[1,x] = 0 /\
    grid[size+2,x] = 0 /\  
    grid[size+3,x] = 0 /\
    grid[x,0] == 0 /\       
    grid[x,1] == 0 /\
    grid[x,size+2] = 0 /\  
    grid[x,size+3] = 0 
  )
  /\
  forall(r in checkConstraintsBoardCoord,c in checkConstraintsBoardCoord) (
    % C2: Each cell of the board (except those of the first/last row/column) 
    %     that has exactly three live neighbors is alive. 
    %     Together with constraint C1, this implies that cells in the
    %     second/last-but-one row/column cannot have three live neighbors.
    (
      ( ( grid[r-1,c-1] + grid[r-1,c] + grid[r-1,c+1] + 
          grid[r,c-1] + grid[r,c+1] + 
          grid[r+1,c-1] + grid[r+1,c] + grid[r+1,c+1]
          ) = 3 
       ) -> (grid[r,c] = 1)
    )
    /\    

    % C3: Each live cell must have 2 or 3 live neighbors (cells of the first/last 
    % row/column may be ignored by this constraint)
    (
       (grid[r,c] = 1) -> 
                      (
                        2 <= 
                        ( grid[r-1,c-1] + grid[r-1,c] + grid[r-1,c+1] +
                          grid[r,c-1] + grid[r,c+1] +
                          grid[r+1,c-1] + grid[r+1,c] + grid[r+1,c+1] 
                          )
                        /\
                        ( grid[r-1,c-1] + grid[r-1,c] + grid[r-1,c+1] +
                          grid[r,c-1] + grid[r,c+1] +
                          grid[r+1,c-1] + grid[r+1,c] + grid[r+1,c+1] 
                          ) <= 3
                      )
    )
  )
  
  /\
  % SBSO: Symmetry-breaking by selective ordering
  % The assignment is forced to respect an ordering on the values that occur in corner entries
  % of the board. In particular:  
  % - if the NW-corner cell is dead, the SE-corner cell
  % must be dead too 
  % - if the NE-corner cell is dead, the SW-corner cell must be dead too
  % 
  grid[2,2] >= grid[size+1,size+1] /\
  grid[2,size+1] >= grid[size+1,2]
;


output [
  if j = 0 then "\n" else " " endif ++
    show(grid[i,j])
  | i,j in augmentedBoardCoord
];
