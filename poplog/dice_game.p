/*

  Dice Game in Pop11.

  From
  http://www.informs.org/ORMS-Today/Public-Articles/December-Volume-38-Number-6/THE-PUZZLOR
  """
  A         B           C           D
     7          4          6            5 
   1 1 1 1    4 4 4 4    2 2 2 2      3 5 3 3
     7          4          6            5
  Figure 1 shows four dice with varying numbers on each face. You and 
  three friends will play a simple game where you will each roll one 
  of the dice and the highest number wins. You get first pick from 
  the dice.

  Which die should you choose in order to maximize your chance of winning?
  """

  Analysis
    A vs B:  A wins 2 times, B wins 4 times
    A vs C:  A wins 2 times, C wins 4 times
    A vs D:  A wins 2 times, D wins 4 times
    B vs C:  B wins 4 times, C wins 2 times
    B vs D:  B wins 3 times, D wins 3 times
    C vs D:  C wins 2 times, D wins 4 times

    Wins (of row vs col)
      A  B  C  D    Sum (winning)
   A  -  2  2  2      6    0.166667
   B  4  -  4  3     11    0.305556   <---
   C  4  2  -  2      8    0.222222
   D  4  4  3  -     11    0.305556   <---
                    --- 
                     36

   Simulation (n = 10000000)

   sum: i > j, sum2: i >= j

** [1 sum 10001725 sum2 10002355]
** [2 sum 18333344 sum2 18335063]
** [3 sum 13331345 sum2 13332250]
** [4 sum 18333191 sum2 18327827]
** [L [10001725 18333344 13331345 18333191]] 
** [L2 [10002355 18335063 13332250 18327827]] 
** [max_sum 18333344 max_i 2] 
** [max_sum2 18335063 max_i2 2] 


   This Pop-11 program was created by Hakan Kjellerstrand (hakank@bonetmail.com
   See also my Pop-11/Poplog page: http://www.hakank.org/poplog/


*/
compile('/home/hakank/Poplib/init.p');


vars game = [
                  [1 1 1 1 7 7] ;;; A
                  [4 4 4 4 4 4] ;;; B
                  [2 2 2 2 6 6] ;;; C
                  [3 5 3 3 5 5] ;;; D
                  ];
     
vars i, j, k;
vars n = 10000;
vars len = game.length;
vars L = [];  ;;; i > j
;;; vars L2 = []; ;;; i >= j
for i from 1 to len do
    lvars sum = 0;
    lvars sum2 = 0;
    for j from 1 to len do
        if i /= j then
            repeat n times
                if oneof(game(i)) > oneof(game(j)) then
                    sum+1->sum;
                endif;
                ;;; if oneof(game(i)) >= oneof(game(j)) then
                ;;;     sum2+1->sum2;
                ;;; endif;
            endrepeat;
        endif;
    endfor;
    [^i sum ^sum sum2 ^sum2]=>
    L<>[^sum]->L;
    ;;; L2<>[^sum2]->L2;
endfor;

[L ^L]=>
;;; [L2 ^L2]=>

vars max_i = 1, max_sum = L(1);
;;; vars max_i2 = 1, max_sum2 = L(1);
for i from 2 to len do
    if L(i) > max_sum then
        i->max_i;
        L(i)->max_sum;
    endif;
    ;;; if L2(i) > max_sum2 then
    ;;;     i->max_i2;
    ;;;     L2(i)->max_sum2;
    ;;; endif;
endfor;

[max_sum ^max_sum max_i ^max_i]=>
;;; [max_sum2 ^max_sum2 max_i2 ^max_i2]=>
