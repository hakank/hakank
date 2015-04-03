% 
% Enigma 95: For love and money in MiniZinc.
% 
% https://enigmaticcode.wordpress.com/2013/06/03/enigma-95-for-love-and-money/
% """
% Faced with seven suitable suitors, Amanda decided to mark them out of 50 for passion and out of 
% 50 for earning power and to total the marks to find the man of her dreams. And so she did. 
% There were no ties on either list and, when totalled, the swains came out in alphabetical order.
% 
% Aloysius came 4th for love and 4th for money. Bertrand totalled 40 marks. Cuthbert was placed 
% 2nd for love. Desmond scored 11 for money and Egbert 15 for love. Ferdie was 5th for love and 
% scored 20 for money. Geoffrey came 6th in the money order. Someone scored for money exactly 
% what Cuthbert scored for love.
% 
% Can you reproduce all Amanda’s mark?s
% """

% 
% This MiniZinc model was created by Hakan Kjellerstrand, hakank@gmail.com
% See also my MiniZinc page: http://www.hakank.org/minizinc/
%

include "globals.mzn"; 

int: n = 7;

int: Aloysius = 1;
int: Bertrand = 2;
int: Cuthbert = 3;
int: Desmond  = 4;
int: Egbert   = 5;
int: Ferdie   = 6;
int: Geoffrey = 7;


% decision variables
array[1..n] of var 0..50: love;
array[1..n] of var 0..50: money;
array[1..n] of var 0..100: total;

solve satisfy;

%
% helper predicate:
%  person "p" is placed in "pos" in array "a"
%
predicate placed(array[int] of var int: a, var int: p, int: pos) =
  sum([a[i] >= a[p] | i in index_set(a)]) = pos
;

constraint

  % "There were no ties on either list "
  alldifferent(love) /\
  alldifferent(money) /\

  % "and, when totalled, the swains came out in alphabetical order."
  alldifferent(total) /\
  decreasing(total) /\

  forall(i in 1..n) ( total[i] = love[i] + money[i]) /\

  % "Aloysius came 4th for love and 4th for money."
  placed(love,Aloysius, 4) /\
  placed(money,Aloysius, 4) /\

  % "Bertrand totalled 40 marks." 
  total[Bertrand] = 40 /\
  
  % "Cuthbert was placed 2nd for love." 
  placed(love,Cuthbert,2) /\

  % "Desmond scored 11 for money" 
  money[Desmond] = 11 /\

  % "and Egbert 15 for love. 
  love[Egbert] = 15 /\

  % "Ferdie was 5th for love and scored 20 for money. "
  placed(love,Ferdie, 5) /\
  money[Ferdie] = 20 /\

  % "Geoffrey came 6th in the money order. "
  placed(money, Geoffrey, 6) /\

  % "Someone scored for money exactly what Cuthbert scored for love."
  exists(i in 1..n) (
    money[i] = love[Cuthbert]
  )

;

output [
  "         A   B   C   D   E   F   G\n",
  "love : ", show(love), "\n",
  "money: ", show(money), "\n",
  "total: ", show(total), "\n",
]
;

