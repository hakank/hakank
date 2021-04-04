/* 

  Giant Cat Army riddle in MiniZinc.
 
  Via https://stackoverflow.com/questions/65511714/about-building-a-list-until-it-meets-conditions

  """
  Basically you start with [0], then you build this list by using one of three 
  operations: adding 5, adding 7, or taking sqrt. You successfully complete the 
  game when you have managed to build a list such that 2,10 and 14 appear 
  on the list, in that order, and there can be other numbers between them.

  The rules also require that all the elements are distinct, they're all <=60 
  and are all only integers. For example, starting with [0], you can
  apply (add5, add7, add5), which would result in [0, 5, 12, 17], but since 
  it doesn't have 2,10,14 in that order it doesn't satisfy the game.
  """

  YouTube: 
  "Can you solve the giant cat army riddle? - Dan Finkel"
  https://www.youtube.com/watch?v=YeMVoJKn1Tg&feature=youtu.be

  Note: I'm not sure if it's required that one should show the operations,
  or if it's sufficient to just show the list of numbers.


  There is 98 solutions for len=24 (which is the smallest length).

 
  This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my MiniZinc page: http://www.hakank.org/minizinc/

*/
include "globals.mzn"; 

int: len;

% decision variables
array[1..len] of var 0..60: x;

% solve satisfy;
solve :: int_search(x, first_fail, indomain_min, complete) satisfy;

constraint
  x[1] = 0 % first value
  /\
  (x[2] = 5 \/ x[2] = 7) % these cannot be sqrt
  /\
  x[len] = 14 % last value must be 14
  /\
  forall(i in 1..len-1) (
     (x[i+1] = x[i] + 5) \/
     (x[i+1] = x[i] + 7) \/
     (x[i+1] <= 7 /\ x[i+1]^2 = x[i])
  )
  /\
  all_different(x)
;

% check the order of 2 ... 10 ... 14
constraint
  
  let {
    var 1..len: ix2;
    var 1..len: ix10;
  } in 
    x[ix2] = 2 /\
    x[ix10] = 10 /\
    ix2 < ix10 
;

output [
  "x: \(x)\n"
];


%
% data
%
len = 24; % smallest length
