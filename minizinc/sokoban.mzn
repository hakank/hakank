% 
% Sokoban in MiniZinc.
% 
% Translation of the Essence' model from
% the Savile Row distribution examples/sokoban.prime
% """
% Sokoban (Japanese for "warehouse keeper") is a transport puzzle. The
% game was invented by Hiroyuki Imabayashi, and was published by
% Thinking Rabbit, a computer games company in the town of Takarazuka,
% Japan. The game is composed of a two dimensional layout of a
% warehouse laid out on a rectangular grid. An empty cell indicates a 
% part of the warehouse floor where the porter can move freely. The 
% walls constitute the boundaries of the warehouse. Some of the floor 
% cells contain crates. Each crate is a 1x1 square occupying a single 
% cell. These crates are to be moved into designated target positions
% by a porter. The number of target positions and the number of crates
% should be the same. The crates cannot be pulled and can be pushed 
% only one at a time. Thus at a time, a cell may be empty, occupied by 
% the porter, a packet, a target position or a part of the warehouse
% wall. 
%
% by Geetha Ramachandran
% geetha.ramachandran@gmail.com
%
% modified by Andrea Rendl
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 



int: w; % the width of the board
int: n; % the amount of fields

int: pInit;
int: stps;

int: noWalls;
%given noInvalidMoves : int
int: noGoals;
int: noCrates;

set of int: WALLINDICES = 0..noWalls-1;
set of int: GOALINDICES = 0..noGoals-1;
set of int: CRATEINDICES = 0..noCrates-1;
set of int: POSITIONS = 0..n-1;

array[WALLINDICES] of int: walls;
array[GOALINDICES] of int: goals;
array[CRATEINDICES] of int: crates;


set of int: MOVES = {-w,-1,1,w};

set of int: STEPS = 0..stps-1;

% decision variables
% Variables to be determined: 
% sokPosn        -  Position of the porter at each time step
% move           - Direction of movement made by the porter at each time step
% crateGoalPosns - Position of each crate at each time step
array[STEPS] of var 0..n-1: sokPosn;
array[0..stps-2] of var MOVES: move;
array[STEPS,CRATEINDICES] of var 0..n-1: crateGoalPosns;

% solve satisfy;
% solve :: int_search(sokPosn ++ move, occurrence, indomain_split, complete) satisfy;
solve :: int_search(sokPosn ++ move, occurrence, indomain_split, complete) satisfy;

constraint

  sokPosn[0] = pInit /\

  forall(i in STEPS) (
    forall(wll in WALLINDICES) (
      sokPosn[i] != walls[wll] 
    )
  )
  /\

  %% moving the porter
  forall(i in 0..stps-2) (
    sokPosn[i+1] = sokPosn[i] + move[i]
  )
  /\
  %% initialization of the crates' positions
  forall(ck in 0..noCrates-1) (
    crateGoalPosns[0,ck] = crates[ck] 
  )
  /\
  forall(s in STEPS) (
    alldifferent([crateGoalPosns[s,j]  | j in CRATEINDICES]) 
  )
  /\
	
  %% moving crates
  forall(k in 0..stps-2) (
    forall(c in CRATEINDICES) (
      sokPosn[k+1] = crateGoalPosns[k,c]  ->
         crateGoalPosns[k+1,c] = crateGoalPosns[k,c] + move[k]
    )
  )
  /\
  forall(i in 0..stps-2) (
    forall(c in CRATEINDICES) (
      sokPosn[i+1] = crateGoalPosns[i,c] 
      \/ 
      crateGoalPosns[i+1,c] = crateGoalPosns[i,c]
    )
  )
  /\
  %% ensuring that each crate arrives at a target

  forall(c in CRATEINDICES) (
    exists(g in GOALINDICES) (
      crateGoalPosns[stps-1,c] = goals[g]
    )
 )
;

output [

  "sokPosn: " ++ show(sokPosn) ++ "\n" ++
  "move: " ++ show(move) ++ "\n" ++
  "crateGoalPosns: " ++ show(crateGoalPosns) ++ "\n"

];


%
% Data
% from sokobanMedium1.param
% 
%
%  # - Wall
%  . - Target Position
%  @ - Porter
%  % - Crate
%    - Empty Cell
%
% First instance from 
% http://users.bentonrea.com/~sasquatch/sokoban/m1
%  
%  012345
% 0####
% 1# .#
% 2#  ###
% 3# @  #
% 4#  % #
% 5#  ###
% 6####
%
% width: 6
% height: 7

w = 6;
n = 42;

noCrates = 1;
noGoals = 1;

pInit = 20;
stps = 14; % don't know how many?

walls = array1d(WALLINDICES, 
                 [ 0,  1,  2,  3,  4,  5,    %  6

                   6,          9, 10, 11,    %  4
    
                  12,         15, 16, 17,    %  4
              
                  18,                 23,    %  2
  
                  24,                 29,    %  2

                  30,         33, 34, 35,    %  4

                  36, 37, 38, 39, 40, 41 ]);  %  6

                                    %       --------
                                    % noWalls:  28
noWalls = 28;

crates = array1d(CRATEINDICES, [27]);
goals = array1d(GOALINDICES, [8]);
