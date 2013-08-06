/*

   """
   3. A man who possesses a half-sovereign, a florin and a sixpence goes into 
   a shop and buys goods worth 7 shillings and 3 pence. But the shopkeeper cannot 
   give him the correct change, as his coins are a crown, a shilling, and a penny. 
   But a friend comes in the shop, and finds that he has a double-florin, a 
   half-crown, a fourpenny piece and a threepenny bit.

   Can the shopkeeper effect an exchange that will enable him to give the 
   man the correct change, and to give his friend the exact equivalent of his 
   coins? (Wakeling)
   """

   ! Description  : Lewis Carroll coin puzzle
   ! Source       : Wakeling, E., (1995), Rediscovered Lewis Carroll Puzzles, Dover. 
   ! Date written : Xpress-MP 7/10/99, Mosel 17/4/03
   ! Written by   : M J Chlond 

Solution:

After transaction coins are distributed as follows:
* Buyer has 1 crown and 1 threepence
* Shopkeeper has 1 half-sovereign, 1 half-crown, 1 sixpence and 1 fourpence
* Friend has 1 double-florin, one florin, 1 shilling and 1 penny

Here:
x [*,*]
:    1   2   3    :=
1    0   1   0
2    1   0   0
3    0   0   1
4    0   0   1
5    0   1   0
6    0   1   0
7    0   0   1
8    0   1   0
9    1   0   0
10   0   0   1

Or some more understandable:
                Buyer   Shopkeeper      Friend
coin:  120      0               1       0
coin:  60       1               0       0
coin:  48       0               0       1
coin:  30       0               0       1
coin:  24       0               1       0
coin:  12       0               1       0
coin:   6       0               0       1
coin:   4       0               1       0
coin:   3       1               0       0
coin:   1       0               0       1


 This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
 See also my AMPL page: http://www.hakank.org/ampl/


*/


param person = 3;
param coin = 10;

set P = 1..person;
set C = 1..coin;
param requ{P};
param value{C};
var x{C,P} binary;

minimize any:
          x[1,1];



subject to reqcon{j in P}:
    sum{i in C} value[i]*x[i,j] = requ[j];

subject to useone{i in C}:
   sum{j in P} x[i,j] = 1;

data;

param requ:= 
     1 63,
     2 160,
     3 85;

param value := 
      1 120,
      2 60,
      3 48,
      4 30,
      5 24,
      6 12,
      7 6,
      8 4,
      9 3,
      10 1
;


option solver cplex;
solve;

display x;

printf "\t\tBuyer\tShopkeeper\tFriend\n";
for{i in C} {
      printf "coin: % 3d\t%d\t\t%d\t%d\n", value[i], x[i,1], x[i,2], x[i,3];
}
