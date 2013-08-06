% 
% Krypto puzzle in MiniZinc.
% 
% From http://en.wikipedia.org/wiki/Krypto_%28game%29
% """
% The Krypto deck consists of 56 cards: three each of numbers 1-6, four each of 
% the numbers 7-10, two each of 11-17, one each of 18-25. Six cards are dealt: 
% a common objective card at the top and five other cards below. Each player must 
% use all five of the cards' numbers exactly once, using any combination of arithmetic 
% operations (addition, subtraction, multiplication, and division), to form the 
% objective card's number. The first player to come up with a correct formula is 
% the winner.
%
% ...
% Example of Play:
%
%      Cards:  2, 1, 2, 2, 3 = 24 (Objective Card)
%      2 x   1 =  2
%      2 x   2 =  4
%      4 x   2 =  8
%      8 x   3 = 24 (Krypto)
%
% All five cards were used once and only once to equal the Objective Card.
%
% Another Example:
%
%      Cards:  1, 3, 7, 1, 8 = 1 (Objective Card)
%      3 -  1 =  2
%      7 +  2 =  9
%      9 /  1 =  9
%      9 -  8 =  1 (Krypto)
%
% Here is a more difficult hand:
%
%      Cards:  24, 22, 23, 20, 21 = 1  (Objective Card)
%      24 + 22 = 46
%      46 / 23 =  2
%       2 + 20 = 22
%      22 - 21 =  1 (Krypto)
% """
%
% Also, see
%   http://www.math.niu.edu/~rusin/uses-math/games/krypto/
%
% Note: This model use a restricted version where we don't handle
%       parenthesis around the some of the arguments.


% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n;
array[1..n] of int: cards;
int: objective;

set of int: cc = {cards[i] | i in 1..n};

% should cardsp be the same order as cardsp
bool: ordered;

% decision variable
array[1..n] of var cc: cardsp;
array[1..n] of var 1..max(max(100,sum(cards)),objective): x;
array[1..n-1] of var 1..4: ops;



predicate opx(var int: x1, var int: x2, var int: c, var int: opp) =
 (x1 = x2 + c /\ opp = 1) \/
 (x1 = x2 - c /\ opp = 2) \/
 (x1 = x2 * c /\ opp = 3) \/
 (c > 0 /\ x2 mod c = 0 /\ x1 = x2 div c /\ opp = 4)
;

% b is a permutation of a
predicate permutation(array[int] of var int: a,
                      array[int] of var int: b) =
    forall(val in {cards[i] | i in 1..n}) (
        let {
          var 1..length(a): c
        } in
        count(a, val, c) /\
        count(b, val, c)
    )
;

% solve satisfy;
solve :: int_search(cardsp ++ x ++ ops, first_fail, indomain_split, complete) satisfy;

constraint
   permutation(cards,cardsp) /\

   x[1] = cardsp[1] /\
   forall(i in 2..n) (
     opx(x[i],x[i-1],cardsp[i],ops[i-1])
   )
   /\
   x[n] = objective

   /\
   if ordered then
     forall(i in 1..n) ( cards[i] = cardsp[i])
   else
     true
   endif
     
;

output [
   "cards : " ++ show(cards) ++ "\n" ++
   "cardsp: " ++ show(cardsp) ++ "\n" ++
   "x     : " ++ show(x) ++ "\n" ++
   "ops   : " ++ show(ops) ++ "\n"
] 
++
[
  show_int(2, x[i-1]) ++ " " ++ 
     if fix(ops[i-1]) = 1 then "+" elseif 
        fix(ops[i-1]) = 2 then "-" elseif 
        fix(ops[i-1]) = 3 then "*" else
         "/" 
     endif ++
     " " ++
     show_int(2, cardsp[i]) ++ " = " ++ show_int(2, x[i]) ++ "\n"
  | i in 2..n
]
;

% 
% data
%
% n = 5;
% cards =[2, 1, 2, 2, 3];
% objective = 24;
% ordered = false;

% n = 5;
% cards=[1, 3, 7, 1, 8];
% objective = 1;
% ordered = false;

% n = 5;
% cards=[24, 22, 23, 20, 21];
% objective = 1;
% ordered = false;


n = 10;
cards=[1,2,3,4,5,6,7,8,9,10];
objective = 2013;
ordered = false;
