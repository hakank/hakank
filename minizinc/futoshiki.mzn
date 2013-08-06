% 
% Futoshiki problem in MiniZinc.
% 
% http://en.wikipedia.org/wiki/Futoshiki
% http://www.guardian.co.uk/world/2006/sep/30/japan.estheraddley
%
% Model from the Minion/Tailor example futoshiki.eprime .
%
 
%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 


int:  SIZE = 5;
int: lastdx; % index of last lt entry (0-based)

set of int: NUMQD = 0..lastdx;
set of int: RANGE = 1..SIZE;
set of int: VALUES = 0..SIZE;

%  the numeric values specified in the puzzle
array[RANGE, RANGE] of VALUES: values;

% list of < relations in the problem
array[NUMQD, 0..3] of RANGE: lt;

array[RANGE, RANGE] of var RANGE: field;


solve satisfy;

constraint
    % all rows have to be different
    forall(row in RANGE) (
        all_different([field[row,col] | col in RANGE])
    )
    /\
    % all columns have to be different
    forall(col in RANGE) (
        all_different([field[row,col] | row in RANGE])
    )
    /\
    % all < constraints are satisfied
    forall(i in NUMQD) (
        ( field[ lt[i,0], lt[i,1] ] < field[ lt[i,2], lt[i,3] ] )

    )
    /\
    % set initial values
    forall(row,col in RANGE) (
        ( values[row,col] > 0) ->
            (field[row,col] = values[row,col] )
    )
;

output [
  if col = 1 then "\n" else " " endif ++
    show(field[row, col])
  | row, col in RANGE
] ++ ["\n"];

%
% Data
%


% Example from Tailor model futoshiki.param/futoshiki.param
%
% Solution:
% 5 1 3 2 4
% 1 4 2 5 3
% 2 3 1 4 5
% 3 5 4 1 2
% 4 2 5 3 1
% 
% Futoshiki instance, by Andras Salamon
% specify the numbers in the grid
values = array2d(RANGE, RANGE, [
                                0, 0, 3, 2, 0,
                                0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0,
                                0, 0, 0, 0, 0]);

% specify last index in array lt; lt[0] is first entry
lastdx = 10;

% [i1,j1, i2,j2] requires that values[i1,j1] < values[i2,j2]
 lt = array2d(NUMQD, 0..3, [
                            1,2,1,1,
                            1,4,1,5,
                            2,3,1,3,
                            3,3,2,3,
                            3,4,2,4,
                            2,5,3,5,
                            3,2,4,2,
                            4,4,4,3,
                            5,2,5,1,
                            5,4,5,3,
                            5,5,4,5
                        ]);


% Example from http://en.wikipedia.org/wiki/Futoshiki
% Solution:
% 5 4 3 2 1
% 4 3 1 5 2
% 2 1 4 3 5
% 3 5 2 1 4
% 1 2 5 4 3
% values = array2d(RANGE, RANGE, [
%                                 0, 0, 0, 0, 0,
%                                 4, 0, 0, 0, 2,
%                                 0, 0, 4, 0, 0,
%                                 0, 0, 0, 0, 4,
%                                 0, 0, 0, 0, 0]);
% lastdx = 5;
% lt = array2d(NUMQD, 0..3, [
%                            1,2, 1,1,
%                            1,4, 1,3,
%                            1,5, 1,4,
%                            4,4, 4,5,
%                            5,1, 5,2,
%                            5,2, 5,3
% ]);
