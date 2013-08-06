/*

http://www.chlond.demon.co.uk/puzzles/puzzles2.html
"""
4. "Mrs Spooner called this morning," said the honest grocer to his assistant. "She wants twenty pounds of tea at 2s. 4 1/2d. per lb. Of course we have a good tea 2s. 6d. tea, a slightly inferior at 2s 3d., and a cheap Indian at 1s. 9d., but she is very particular always about her prices."

"What do you propose to do?" asked the innocent assistant.

"Do?" exclaimed the grocer. "Why, just mix up the three teas in different proportions so that the twenty pounds will work out fairly at the lady's price. Only don't put in more of the best tea than you can help, as we make less profit on that, and of course you will use only our complete pound packets. Don't do any weighing."

How was the poor fellow to mix the three teas? Could you have shown him how to do it? (Dudeney)    Solution 
"""

! Description  : Dudeney's tea mixing problem
! Source       : Dudeney, H.E., (1917), Amusements in Mathematics, Thomas Nelson and Sons.  
! Date written : Xpress-MP 18/10/99, Mosel 17/4/03
! Written by   : M J Chlond 

Solution:
10 at 2s. 6d. and 10 at 2s. 3d.

Here:

x [*] :=
1  10 
2  10     
3   0
;

Price 30 (2s 4): 10
Price 27 (2s 6d): 10
Price 21 (1s 9d): 0


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/

param tea := 3;

set T := 1..tea;

param price{T};
param priceName{T} symbolic; # price name
var x{T} integer >= 0;

minimize minexp:
      sum{i in T} x[i]*price[i]; # minimize the total price
#     x[1];  # original

# 1s = 12
subject to pcon:
  sum{i in T} price[i]*x[i] = 570; # 20 pound * 2s 4 1/2d = 20 * (24+4+1/2) = 570

subject to wcon:
  sum{i in T} x[i] = 20; # twenty pound


data;

param price:= 
   1 30
   2 27
   3 21
;

param priceName:=
   1 "2s 6d" 
   2 "2s 4d"
   3 "1s 9d"
;

option solver cplex;
solve;

display x;

for{i in T} {
   printf "Price %d (%s): %d\n", price[i], priceName[i], x[i];
}

quit;
