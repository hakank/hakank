/*

http://www.chlond.demon.co.uk/puzzles/puzzles4.html
http://www.chlond.demon.co.uk/puzzles/sol4s3.html

"""
3. A Logical Labyrinth

A prisoner is faced with a decision where he must open one of nine doors. The rooms behind each door may be empty or contain either a lady or a tiger.

If the prisoner opens a door to find a lady he will marry her and if he opens a door to find a tiger he will be eaten alive. The prisoner would prefer to be married than either be eaten alive or to face emptiness. Each door has a sign bearing a statement which may be either true or false.

The statements on the nine doors are:
1. The lady is an odd-numbered room
2. This room is empty
3. Either sign 5 is right or sign 7 is wrong
4. Sign 1 is wrong
5. Either sign 2 or sign 4 is right
6. Sign 3 is wrong
7. The lady is not in room 1
8. This room contains a tiger and room 9 is empty
9. This room contains a tiger and sign 6 is wrong

In addition, the prisoner is informed that only one room contains a lady; each of the others either contain a tiger or are empty. The sign on the door of the room containing the lady is true, the signs on all the doors containing tigers are false, and the signs on the doors of empty rooms can be either true or false.

The prisoner is told whether or not room eight is empty and this knowledge helps him find a unique solution.(Smullyan)    Solution )
"""


Xpress-Mosel Model

model 'trial12'

! Description  : The Logical Labyrinth
! Source       : Smullyan, R., (1991), The Lady or The Tiger, Oxford University Press
! Date written : Xpress-MP 21/12/99, Mosel 19/4/03
! Written by   : M J Chlond 

Solution
The lady is in Room Seven. Note: If room 8 was empty then there is not enough information to identify a unique location for the lady. Therefore, the king must have informed the prisoner that room 8 was not empty. This may be verified by experimentation with the following model.


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/

*/

param door  = 9;
param prize = 3; # 1 = Lady, 2 = Tiger, 3 = Empty
  
var x{1..door,1..prize} binary; #  x(i,j) = 1 if door i hides prize j, else 0
var t{1..door} binary;  #  t(i) = 1 if statement on door i is true, else 0

minimize any:
        x[1,1];

# if statement on door 1 is true (i.e. x[1,1]+x[3,1]+x[5,1]+x[7,1]+x[9,1] = 1 ] 
#                                       then t(1] = 1, else t(1] = 0
subject to lca:
        t[1] = x[1,1]+x[3,1]+x[5,1]+x[7,1]+x[9,1];

# if statement on door 2 is true (i.e. x[2,3]=1] then t[2] = 1, else t[2] = 0
subject to lcb: 
        t[2] = x[2,3];

# if statement on door 3 is true (i.e. t[5]+x[1,1] > 1 ] then t[3] = 1, else t[3] = 0
subject to lcc1: 
t[5]+x[1,1]-2*t[3] <= 0;

subject to lcc2: 
        t[5]+x[1,1]-t[3] >= 0;

# if statement on door 4 is true (i.e. t[1] = 0] then t[4] = 1, else t[4] = 0
subject to lcd:
        t[4] = 1-t[1];

# if statement on door 5 is true (i.e. t[2]+t[4] > 1] then t[5] = 1, else t[5] = 0
subject to lce1:
        t[2]+t[4]-2*t[5] <= 0;

subject to lce2:
        t[2]+t[4]-t[5] >= 0;

# if statement on door 6 is true (i.e. t[3] = 0 ] then t[6] = 1, else t[6] = 0
subject to lcf:
        t[6] = 1-t[3];

# if statement on door 7 is true (i.e. x[1,1] = 0] then t[7] = 1, else t[7] = 0
subject to lcg:
        t[7] = 1-x[1,1];

# if statement on door 8 is true (i.e. x[8,2]+x[9,3] = 2 ] then t[8] = 1, else t[8] = 0
subject to lch1:
        x[8,2]+x[9,3]-2*t[8] <= 1;
subject to lch2:
        x[8,2]+x[9,3]-2*t[8] >= 0;

# if statement on door 9 is true (i.e. x[9,2]+t[3] = 2] then t[9] = 1, else t[9] = 0
subject to lci1:
        x[9,2]+t[3]-2*t[9] <= 1;
subject to lci2:
        x[9,2]+t[3]-2*t[9] >= 0;

# each door hides 1 prize
subject to pca{i in 1..door}:
        sum{j in 1..prize} x[i,j] = 1 ;

# only one room contains lady
subject to pcb:
        sum{i in 1..door} x[i,1] = 1;

# sign on lady's door is true
subject to lck{i in 1..door}:
        t[i] >= x[i,1] ;

# sign on tigers' doors are false
subject to lcl{i in 1..door}:
        t[i] <= 1 - x[i,2];


# if room 8 is empty then not enough information to pinpoint lady
# min and max x[7,1] give different results

# room 8 is empty
# subject to pcc:
#        x[8,3] = 1; # 

# if room 8 is not empty then enough information
# min and max x[7,1] gives same results
# if the prisoner was able to deduce where the lady was then
# room 8 must not have been empty

# room 8 is not empty
subject to pcc:
        x[8,3] = 0;

# option cplex_options 'sensitivity';
# option solver cplex;
option solver cbc;
solve;

# display results

display x;
display t;


printf "The lady is in room: ";
for{i in 1..door: x[i,1] > 0} print i; 

display _solve_elapsed_time;