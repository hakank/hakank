% 
% Nontransitive dice in MiniZinc.
% 
% From 
% http://en.wikipedia.org/wiki/Nontransitive_dice
% """
% A set of nontransitive dice is a set of dice for which the relation 
% "is more likely to roll a higher number" is not transitive. See also 
% intransitivity.
% 
% This situation is similar to that in the game Rock, Paper, Scissors, 
% in which each element has an advantage over one choice and a 
% disadvantage to the other.
% """
%
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@bonetmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc
%

include "globals.mzn"; 
int: m = 6; % number of dice
int: n = 6; % number of sides of each die

% the dice
array[1..m, 1..n] of var 1..n*2: dice;

% the competitions: 
% (die 1 vs die 2, die 2 vs die 1), ... (die m vs die 1, die 1 vs die m)
% And the first die must beat the second in each round.
% Note the last wrap around which breaks the transitivity.
array[0..m-1, 1..2] of var 0..n*n: comp;

% max value of the dice
var 0..n*2: max_val;
% max win
var 0..n*n: max_win;

% gap, gap_sum
array[0..m-1] of var 0..n*n: gap;
var 0..n*n*m: gap_sum = sum(gap);

% sums of the dice (experimental)
% array[1..m] of var 0..m*n: sums;

% arrays of 1d where both arguments are var int
predicate cp1d(array[int] of var int: x, array[int] of var int: y) =
  assert(index_set(x) = index_set(y),
           "cp1d: x and y have different sizes",
     forall(i in index_set(x)) ( x[i] = y[i] ))
; 



% solve satisfy;
solve :: int_search(
        [dice[i,j] | i in 1..m, j in 1..n] ++ [comp[i,j] | i in 0..m-1, j in 1..2] 
        % ++ [max_win, max_val]
        , 
        first_fail, % occurrence, 
        indomain_median,
        complete) 
    % satisfy;
    minimize max_val; % minimize the largest value of the dice
    % maximize max_win; % maximize the largest winning
    % maximize gap_sum; % maximize the sum of the gaps
    % maximize sum([comp[i,j] | i in 0..m-1, j in 1..2]);
    % maximize sum([comp[i,1] | i in 0..m-1]); % maximize the sum of the winnings
    % maximize min([comp[i,1] | i in 0..m-1]); % maximize the smallest winning
    % maximize min(gap); % maximize the smallest winning


constraint
   maximum(max_val, [dice[i,j] | i in 1..m,j in 1..n]) /\
   % max_val = 5 /\ % for 3 dice

   maximum(max_win, [comp[i,j] | i in 0..m-1, j in 1..2]) /\

   % order the number of each die
   forall(d in 1..m) (
       increasing([dice[d,i] | i in 1..n])
   )

   %/\ % sort the dice (lex greater)
   %forall(d in 2..m) (
   %   lex_greater([dice[d-1,i] | i in 1..n], [dice[d,i] | i in 1..n])
   %)

   /\
   % and now we roll...
   % Number of wins for [A vs B, B vs A]
   forall(d in 0..m-1) (
      comp[d,1] = sum(r1, r2 in 1..n) (bool2int(dice[1+(d mod m), r1] >
                                                dice[1+((d + 1) mod m), r2]))
      /\
      comp[d,2] = sum(r1, r2 in 1..n) (bool2int(dice[1+((d+1) mod m), r1] >
                                                dice[1+((d) mod m), r2]))

      /\
      gap[d] = comp[d,1] - comp[d,2]
   )

   % /\ % Experimental: All dice has the same sums
   % forall(d in 1..m) (
   %    sums[d] = sum([dice[d,i] | i in 1..n])
   %    /\
   %    if d > 1 then
   %      sums[d] = sums[d-1]
   %    else 
   %      true
   %    endif
   % )


   %/\ % from the Wikipedia page
   %   % A > B = 5/9   B > C = 5/9  C > A = 5/9
   %cp1d([dice[1,i] | i in 1..n], [2,2,4,4,9,9]) /\ % die A
   %cp1d([dice[2,i] | i in 1..n], [1,1,6,6,8,8]) /\ % die B
   %cp1d([dice[3,i] | i in 1..n], [3,3,5,5,7,7])    % die C



   % /\ % From Numberjack tutorial, page 32 (slide 67/175)
   % cp1d([dice[1,i] | i in 1..n], [1,2,3,4,5,5]) /\ % die A
   % cp1d([dice[2,i] | i in 1..n], [3,3,3,3,3,3]) /\ % die B
   % cp1d([dice[3,i] | i in 1..n], [2,2,2,3,6,6])    % die C



   % /\ % Efron's 4 dice, the number of each die are re-ordered
   %    % (from the Wikipedia page)
   % cp1d([dice[1,i] | i in 1..n], [0,0,4,4,4,4]) /\ %  A
   % cp1d([dice[2,i] | i in 1..n], [3,3,3,3,3,3]) /\ %  B        
   % cp1d([dice[3,i] | i in 1..n], [2,2,2,2,6,6]) /\ %  C
   % cp1d([dice[4,i] | i in 1..n], [1,1,1,5,5,5])    %  D

   %/\ % Miwin's dice (3 dice)
   %    % Miwin's Dice were invented in 1975 by the physicist Michael Winkelmann.
   %    % (from the Wikipedia page)
   %cp1d([dice[1,i] | i in 1..n], [1,2,5,6,7,9]) /\  % die III
   %cp1d([dice[2,i] | i in 1..n], [1,3,4,5,8,9]) /\ % die IV
   %cp1d([dice[3,i] | i in 1..n], [2,3,4,6,7,8])  % die V


   /\ % non-transitivity
   % All dice 1..m-1 must beat the follower, and die m must beat die 1
   % forall(d in 0..m-1) (
   %    comp[d] > n*n div 2
   %)
   forall(d in 0..m-1) (
       comp[d,1] > comp[d,2]
   )
 
   % /\ % symmetry breaking (be careful if you use one of the examples above)
   % lex2(dice)

   % /\ % extra: all wins are the same
   % forall(d in 1..m-1) (
   %     comp[d,1] = comp[d-1,1]
   %     % comp[d,1] - comp[d,2] = comp[d-1,1] - comp[d-1,2]
   % )

;


output 
[
  "max_val: " ++ show(max_val) ++ "\n" ++
  "max_win: " ++ show(max_win) ++ "\n" ++
  "gap    : " ++ show(gap) ++ "\n" ++
  "gap_sum: " ++ show(gap_sum) ++ "\n" ++
  "comp:"
]
++
[
  if j = 1 then "\n" else " " endif ++
    show_int(2, comp[i,j]) ++ " (" ++ show_float(2, 2, int2float(100*comp[i,j] div sum([comp[i,k] | k in 1..2]))) ++ "%) " 
  | i in 0..m-1, j in 1..2
]
++
[ 
  "\nDice: "
]
++
[
   if j = 1 then "\n" else " " endif ++
   show_int(2, dice[i,j])
  | i in 1..m, j in 1..n
]
% ++
% ["\nSums:\n" ]
% ++
% [
%   show(d) ++ ": " ++ show(sums[d])++"\n"
%   | d in 1..m
% ]
;