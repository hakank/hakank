%
% Social golfer in Minizinc.
% 
% Translated from OPL code from
%
% http://www.dis.uniroma1.it/~tmancini/index.php?currItem=research.publications.webappendices.csplib2x.problemDetails&problemid=010
%
% """
% Problem description
% In a golf club there are 32 social golfers who play once a week in 8 groups of 4. The problem amounts to find a schedule for as many as possible weeks, such that no two golfers play in the same group more than once.

% Here we consider the decisional version of the problem (wrt the number of weeks 'weeks'), where the number of players and the group size are given as input.
% 
% Problem input
% 
% * groups, the number of groups to be formed each week
% * groupSize, the size of each group
% * weeks, the number of weeks for which a scheduling is requested 
% 
% Search space
% The set of all possible group assignments to all players in each of the weeks weeks.
% 
% Constraints
%
% * C1: Each group has exactly groupSize players
% * C2: Each pair of players only meets at most once
% """
%
% Model created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

int: weeks = 4;
int: groups = 3;
int: groupSize = 3;
int: golfers = groups * groupSize;

set of int: Golfer = 1..golfers;
set of int: Week = 1..weeks;
set of int: Group = 1..groups;

% Search space: The set of all possible group assignments to all 
% players in each of the weeks
array[Golfer, Week] of var Group: assign;

% solve satisfy;
% solve :: int_search([assign[i,j] | i in Golfer, j in Week ], "first_fail", "indomain", "complete") satisfy;
solve :: int_search([assign[i,j] | i in Golfer, j in Week ], 
        first_fail, indomain_min, complete) satisfy;

constraint
   % C1: Each group has exactly groupSize players
   forall (gr in Group, w in Week)( % c1
     sum (g in Golfer) (bool2int(assign[g,w] = gr)) = groupSize
   )
   /\
   % C2: Each pair of players only meets at most once
   forall (g1, g2 in Golfer, w1, w2 in Week  where g1 != g2 /\ w1 != w2) (
     (bool2int(assign[g1,w1] = assign[g2,w1]) + bool2int(assign[g1,w2] = assign[g2,w2])) <= 1
   )
  /\
  % SBSA: Symmetry-breaking by selective assignment
  % On the first week, the first groupSize golfers play in group 1, the 
  % second groupSize golfers play in group 2, etc. On the second week, 
  % golfer 1 plays in group 1, golfer 2 plays in group 2, etc.
  forall(g in Golfer) (
    assign[g,1]=((g-1) div groupSize) + 1 % 
  )
  /\
  forall(g in Golfer where g <= groupSize)(
    assign[g,2]=g
  )

;

output [
  if j = 1 then "\n" else " " endif ++
  show(assign[i,j])
  | i in Golfer, j in Week
] ++ ["\n"];


