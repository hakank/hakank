% 
% Loading a pair of dice in MiniZinc.
% 
% From Mind Your Decisions, "Monday Puzzle: Loading a Pair of Dice":
% http://mindyourdecisions.com/blog/2014/10/06/monday-puzzle-loading-a-pair-of-dice/
% """
% In a standard die, each of the six outcomes 1, 2, 3, 4, 5, 6 shows up with equal chance. However, 
% it is physically possible to rig the die so that some numbers show up more frequently. 
% In this problem, assume you can rig a die to have an arbitrary probability distribution.
% 
% Here’s the puzzle: can you rig a pair of dice so that the rolls of 2, 3, 4, …, 12 all have 
% same chance of occurring?
%
% [A lot of mathematics....]
% 
% This is a contradiction, and therefore it is not possible to rig the die to make an 
% equiprobable sum distribution.
% """

%
% One assumption is that we use standard dice (with 1..6 spots), then it's not possible 
% to rig the dice. 
% 
% However, if we allow that a die can contain 0 (zero) spot, then the following would give 
% the same distribution of 2..12:
%
%  x: [0, 0, 0, 6, 6, 6]
%  y: [1, 2, 3, 4, 5, 6]
%  z: [3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3]
%
% Yes, this is cheating. But so is rigging a die in the first place. :-)
% 
% Notes: This works only for even n. The occurrences of each resulting numbers is
%        n / 2 and the following construction of the dice:
%           Notation: a x b means that a is repeated b times
%
%            die 1:[ (0 x n/2), (n x n/2)]
%            die 2:[ 1..n ]
% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n; % = 6; % number of sides of a die


% Original assumption (a die have the spots 1..n):
% -> UNSATISFIABLE
% array[1..n] of var 1..n: x; % first die
% array[1..n] of var 1..n: y; % second die

% If one or both dice can contain zero:
array[1..n] of var 0..n: x; % first die
array[1..n] of var 0..n: y; % second die

array[2..n*2] of var 0..n*n: z; % distribution
var 0..n*n: s = sum(z);

% solve satisfy;
solve :: int_search(z, first_fail, indomain_split, complete) satisfy;

constraint
  forall(i in 2..n*2) (
    z[i] = sum([bool2int(x[j]+y[k] = i)  | j in 1..n, k in 1..n])
  )
  /\ % same chance of recurring
  forall(i in 3..n*2) ( 
      z[i] = z[i-1] 
  )

  /\ s > 0

  % symmetry breaking
  /\ lex_lesseq(x,y) 
  /\ increasing(x) 
  /\ increasing(y)
;

output [
  "x: ", show(x), "\n",
  "y: ", show(y), "\n",  
  "z: ", show(z), "\n",
  "s: ", show(s), "\n",
];

