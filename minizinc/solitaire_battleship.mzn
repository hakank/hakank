% Solitaire Battleship in MiniZinc.
%
% This model is a translation of the ESSENCE' model 
% in the Minion Translator examples:
%
% http://www.cs.st-andrews.ac.uk/~andrea/examples/solitaire_battleship/solitaire_battleship.cm
% """
%
% Popular 2-players game reduced to 1 player:
% Find the battleship, the 2 cruisers, 3 destroyers
% and 4 submarines on an 10x10 grid, given some hints
% for their locations.
%
% (This model is a slightly simplified version of the problem,
% since it does not specify the amount of each shiptype.)
% """
% 
% Also see:
% http://www.dcs.st-and.ac.uk/~ianm/CSPLib/prob/prob014/index.html
% http://en.wikipedia.org/wiki/Battleship_(puzzle)
%
% Barbara Smith: " Constraint Programming Models for Solitaire Battleships."
% http://www.dcs.st-and.ac.uk/~cppod/publications/reports/cppod-20-2006.pdf
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

%
% hakank: All the comments (and TODOs) below are from the ESSENCE' model.
%

set of int: GRID_RANGE = 1..10;
set of int: SQUARE_STATUS = 0..7;
% square status: 0:water,  1:circle, 2:middle, 3:top,
%                4:bottom, 5:right,  6:left, 7:no hint
         
% the amount of occupied squares for each column/row
array[GRID_RANGE] of 0..10: column_tallies;
array[GRID_RANGE] of 0..10: row_tallies;
array[GRID_RANGE,GRID_RANGE] of SQUARE_STATUS: hint;

array[GRID_RANGE, GRID_RANGE] of var 0..1: grid;

solve satisfy;

constraint 
    % amount of occupied sqares in each row
    forall(row in GRID_RANGE) (
        sum(col in GRID_RANGE)  (
            grid[row,col]) = row_tallies[row]
    )
    /\

    % amount of occupied sqares in each column
    forall(col in GRID_RANGE) (
       sum(row in GRID_RANGE) (
            grid[row,col]) = column_tallies[col]
    )
    /\

    % if we have a water hint, then the square is empty	    
    forall(col,row in GRID_RANGE) (
       (hint[row,col] = 0) ->
          (grid[row,col] = 0)
    )
    /\
    %  W W W 
    %  W c W 
    %  W W W 
    %
    % if we have a circle hint (i.e. a submarine) then the square
    % is occupied an all other square are around are empty
    forall(col,row in GRID_RANGE) (
       (hint[row,col] = 1) ->             % hint 1:circle
          (grid[row,col] = 1)
    )
    /\
    forall(row,col in GRID_RANGE) (
       (hint[row,col] = 1) ->            
	  % all square around the submarine are water
         (   ((col-1 >0) -> 
      	   (grid[row-1,col-1] =   0)) /\
             (((row-1 >0) /\ (col-1 >0)) -> 
           (grid[row-1,col-1] = 0)) /\
             (((row-1 >0) /\ (col+1 <11)) -> 
           (grid[row-1,col+1] = 0)) /\
             ((row+1 <11) -> 
           (grid[row+1,col] =   0)) /\
             ((row-1 >0) -> 
           (grid[row-1,col] =   0)) /\
             ((col+1 <11) -> 
           (grid[row,col+1] =   0)) /\
             (((col+1 <11) /\ (row+1 <11)) -> 
           (grid[row+1,col+1] = 0)) /\
             (((row-1 >0) /\ (col+1 <11)) -> 
           (grid[row-1,col+1] = 0))
          )
     )
     /\


    %  W ? W
    %  ? m ?
    %  W ? W	
    %            
    % we have a middle-hint (there is still more to deduct from that)
    forall(row,col in GRID_RANGE) (
        (hint[row,col] = 2) ->	           %hint 2:middle
           ((grid[row,col] = 1) /\
	   
	      % the diagonal squares are water for sure
              (((row-1 >0) /\ (col-1 >0)) -> 
             (grid[row-1,col-1] = 0)) /\ 
               (((row+1 <11) /\ (col-1 >0)) -> 
             (grid[row+1,col-1] = 0)) /\ 
               (((row+1 <11) /\ (col+1 <11)) -> 
             (grid[row+1,col+1] = 0)) /\ 
               (((row-1 >0) /\ (col+1 <11)) -> 
             (grid[row-1,col+1] = 0)) /\
	     
              % the battleships is either horizontal or vertical
             (  (    ((row-1 >0) ->
                 (grid[row-1,col] = 1)) /\
                     ((row+1 <11) -> 
                 (grid[row+1,col] = 1)))
             \/ (  ((col+1 <11) -> 
                 (grid[row,col+1] = 1)) /\
                   ((col-1 >0) ->
                 (grid[row,col-1] = 1)))
              )
           )
    )
    /\
              

    % W W W    
    % W T W 
    % W x W
    % W ? W
    forall(row,col in GRID_RANGE) (
        (hint[row,col] = 3) ->          %hint 3:top
           ((grid[row,col] = 1) /\
              ((row+1<11) ->
            (grid[row+1,col] = 1)) /\

             (((col-1 >0) /\ (row+2 <11)) -> 
           (grid[row+2,col-1] = 0)) /\
             (((col-1 >0) /\ (row+1 <11)) -> 
           (grid[row+1,col-1] = 0)) /\
             ((col-1 >0) -> 
      	   (grid[row,col-1] =   0)) /\
             (((col-1 >0) /\ (row-1 >0)) -> 
           (grid[row-1,col-1] = 0)) /\
             ((row-1 >0) -> 
           (grid[row-1,col] =   0)) /\
             (((col+1 <11) /\ (row-1 >0)) -> 
           (grid[row-1,col+1] = 0)) /\
             ((col+1 <11) -> 
           (grid[row,col+1] =   0)) /\
             (((col+1 <11) /\ (row+1 <11)) -> 
           (grid[row+1,col+1] = 0)) /\
             (((col+1 <11) /\ (row+2 <11)) -> 
           (grid[row+2,col+1] = 0))

            
           )
     )
    /\

    % W ? W    
    % W x W 
    % W B W
    % W W W
    forall(row,col in GRID_RANGE) (
        (hint[row,col] = 4) ->          %hint 4:bottom
           ((grid[row,col] = 1) /\
              ((row-1>0) ->
            (grid[row-1,col] = 1)) /\


             (((col-1 >0) /\ (row+1 <11)) -> 
           (grid[row+1,col-1] = 0)) /\
             ((col-1 >0) -> 
      	   (grid[row,col-1] =   0)) /\
             (((col-1 >0) /\ (row-1 >0)) -> 
           (grid[row-1,col-1] = 0)) /\
             (((col-1 >0) /\ (row-2 >0)) -> 
           (grid[row-2,col-1] = 0)) /\
             (((col+1 <11) /\ (row-2 >0)) -> 
           (grid[col+1,row-2] = 0)) /\
             (((col+1 <11) /\ (row-1 >0)) -> 
           (grid[row-1,col+1] = 0)) /\
             ((col+1 <11) -> 
           (grid[row,col+1] =   0)) /\
             (((col+1 <11) /\ (row+1 <11)) -> 
           (grid[row+1,col+1] = 0)) /\
             ((row+1 <11) -> 
           (grid[row+1,col] =   0))
            
           )
    )
    /\   

   % W W W W
   % ? x R W
   % W W W W
   %  
      forall(row,col in GRID_RANGE) (
        (hint[row,col] = 5) ->          %hint 5:right
           ((grid[row,col] = 1) /\
              ((col-1>0) ->
            (grid[row,col-1] = 1)) /\


             (((col-1 >0) /\ (row+1 <11)) -> 
           (grid[row+1,col-1] = 0)) /\
             (((col-2 >0) /\ (row+1 <11)) -> 
      	   (grid[row+1,col-2] = 0)) /\
             (((col-2 >0) /\ (row-1 >0)) -> 
      	   (grid[row-1,col-1] = 0)) /\
             (((col-1 >0) /\ (row-1 >0)) -> 
           (grid[row-1,col-1] = 0)) /\
             ((row-1 >0) -> 
           (grid[row-1,col] = 0)) /\
             (((col+1 <11) /\ (row-1 >0)) -> 
           (grid[row-1,col+1] = 0)) /\
             ((col+1 <11) -> 
           (grid[row,col+1] =   0)) /\
             (((col+1 <11) /\ (row+1 <11)) -> 
           (grid[row+1,col+1] = 0)) /\
             ((row+1 <11) -> 
           (grid[row+1,col] =   0))
            
           )
       )
    /\




   % W W W W
   % W L x ?
   % W W W W
   %  
      forall(row,col in GRID_RANGE) (
        (hint[row,col] = 6) ->          %hint 5:left
           ((grid[row,col] = 1) /\
              ((col+1 <11) ->
            (grid[row,col+1] = 1)) /\

             (((col-1 >0) /\ (row+1 <11)) -> 
           (grid[row+1,col-1] = 0)) /\
             ((col-1 >0) -> 
           (grid[row,col-1] = 0)) /\
             (((col-1 >0) /\ (row-1 >0)) -> 
           (grid[row-1,col-1] = 0)) /\
             ((row-1 >0) -> 
           (grid[row-1,col] = 0)) /\
             (((col+1 <11) /\ (row-1 >0)) -> 
           (grid[row-1,col+1] = 0)) /\
             (((col+2 <11) /\ (row+1 <11)) -> 
      	   (grid[row+1,col+2] = 0)) /\
             (((col+2 <11) /\ (row-1 >0)) -> 
      	   (grid[row-1,col+2] = 0)) /\
             (((row+1 <11) /\ (col+1 <11)) -> 
           (grid[row+1,col+1] = 0)) /\
             ((row+1 <11) -> 
           (grid[row+1,col] =   0))
            
           )
      )

  %,



  % TODO: - amount of cruisers, submarines, etc
  %       - no ship may be going around the corner (no edges)
         

  % amount of types of battleships

  % amount of destroyers
 % sum row, col : int(2..9) .
  %   ((grid[row,col] = 1) /\ 
  %       (((grid[row+1,col] = 1) /\
  %         (grid[row+2,col] = 0)
  %       )
  %         \/
  %        (grid[row-1,col] = 1) \/
  %        (grid[row,col+1] = 1) \/
   %        (grid[row,col-1] = 1)
  %       )
     
  %   )
;


output [
  if col = 1 then "\n" else " " endif ++
   show(grid[row, col])
  | row, col in GRID_RANGE
] ++ ["\n"];

%
% data from solitaire_battleship1.param
%
%  Hints for the following instance of 
%  the solitaire battleship problem:
%
%   1 0 0 0 0 0 0 0 0 0 
%   0 0 0 1 0 0 1 1 1 1 
%   0 0 0 1 0 0 0 0 0 0 
%   0 0 0 1 0 0 0 1 1 1 
%   0 0 0 0 0 0 0 0 0 0 
%   1 1 0 0 0 0 1 0 0 0 
%   0 0 0 0 0 0 1 0 0 0 
%   0 0 0 0 0 0 0 0 0 1 
%   0 1 0 1 0 1 0 0 0 0 
%   0 0 0 1 0 0 0 0 0 0 
%
column_tallies = [2, 2,0, 5, 0, 1, 3, 2, 2, 3];
row_tallies = [1, 5, 1, 4, 0, 3, 1, 1, 3, 1];

hint = 
array2d(GRID_RANGE,GRID_RANGE, [
 7,7,7,7,7,7,7,7,7,7,
 7,7,7,3,7,7,6,7,2,7,
 7,7,7,7,7,7,7,7,7,7,
 7,7,7,4,7,0,7,6,7,5,
 7,7,7,7,7,7,7,7,7,7,
 7,5,7,0,7,7,3,7,7,7,
 7,7,7,7,7,7,7,7,7,7,
 7,7,7,7,7,7,7,7,7,1,
 7,1,7,3,7,7,7,7,7,7,
 7,7,7,7,7,7,7,7,7,7
])
;


