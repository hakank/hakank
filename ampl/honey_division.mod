/*

http://www.chlond.demon.co.uk/puzzles/puzzles1.html
"""
6. Once upon a time there was an aged merchant of Baghdad who was much respected by all who knew him. He had three sons, and it was a rule of his life to treat them all exactly alike. Whenever one received a present, the other two were each given one of equal value. One day this worthy man fell sick and died, bequeathing all his possessions to his three sons in equal shares.

The only difficulty that arose was over the stock of honey. There were exactly twenty-one barrels. The old man had left instructions that not only should every son receive an equal quantity of honey, but should receive exactly the same number of barrels, and that no honey should be transferred from barrel to barrel on account of the waste involved. Now, as seven of these barrels were full of honey, seven were half full, and seven were empty, this was found to be quite a puzzle, especially as each brother objected to taking more than four barrels of the same description - full, half full, or empty.

Can you show how they succeeded in making a correct division of the property? (Dudeney)    Solution 
"""

This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/

param son = 3;
param cap = 3;

set S = 1..son;
set C = 1..cap;
param howfull{C};
var x{S,C} integer, >= 1, <= 4;

minimize any:
         sum{i in S, j in C} x[i,j];
#           x[3,3];
#          x[1,1]; # 

#  each son gets 7 barrels
subject to nocon{i in S}:
        sum{j in C} x[i,j] = 7;

#  each son gets 3.5 units
subject to amcon{i in S}:
    sum{j in C} howfull[j]*x[i,j] = 3.5;

#  use 7 of each barrel capacity
subject to nccon{j in C}:
    sum{i in S} x[i,j] = 7;


data;
param howfull:= 
  1 1
  2 0.5
  3 0;

# option solver cplex;
option solver cbc;
solve;

display x;
for{i in S} {
   printf "Son %d: ", i;
   for{j in C} {
      printf "%d ", x[i,j];
   }
   printf "\n";
}

printf "\nSolution should be:\n";
print "3      1       3";
print "2      3       2";
print "2      3       2";


#  forall(i in S) do
#    forall(j in C)
#      write(getsol(x(i,j)),' ')
#    writeln

