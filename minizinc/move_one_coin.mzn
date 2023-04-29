/* 

  Move one coin problem in MiniZinc.

  Problem from Scam Nation
  "Can You Solve This Puzzle Faster Than Brian? (w/ Wayne Hoffman)"
  https://www.youtube.com/watch?v=R0PpXI38zuo

  From this configuration of coins

     o oo ooo oooo

  move exacly one coin to yield a line of coins in the reverse order,
  i.e. the number of collected coins from left is 4, 3, 2, and 1.


  (See below for the unique solution.)


  This model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my MiniZinc page: http://www.hakank.org/minizinc/

*/


include "globals.mzn"; 

int: n = 13;
array[1..n] of int: init = [1,0,1,1,0,1,1,1,0,1,1,1,1];  % 0 represents empty position
array[1..n] of int: goal = [1,1,1,1,0,1,1,1,0,1,1,0,1];

% decision variables
var 1..n: from;
var 1..n: to;


solve satisfy;

constraint
  init[from]= 1 /\
  init[to] = 0 /\
  from != to /\
  forall(k in 1..n) (
    if k != from /\ k != to then
      goal[k] = init[k]
    endif
  )
;

output [
        "Move the coin in position \(from) to empty position \(to)\n",
];

/*

  Here's the unique solution:   

  Move the coin in position 12 to empty position 2
  ----------
  ==========


   1234567890123    positions
   -------------
   o oo ooo oooo    init
              |
              |     position 12
     _________v
    |
    v               position 2
   oooo ooo oo o    goal
  


 */
