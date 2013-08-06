#
# From http://www.mcs.vuw.ac.nz/courses/OPRE251/2006T1/Labs/
#    http://www.mcs.vuw.ac.nz/courses/OPRE251/2006T1/Labs/lab09.pdf
#
# """
# My diet requires that all the food I eat come from one of the four .basic 
# food groups. (chocolate cake, ice cream, soft drink, and cheesecake). 
# Each (large) slice of chocolate cake costs 50c, 
# each scoop of chocolate ice cream costs 20c, 
# each bottle of cola costs 30c, 
# and each piece of pineapple cheesecake costs 80c. 

# Each day, I must ingest at least 500 calories, 
# 6 oz of chocolate, 
# 10 oz of sugar, 
# and 8 oz of fat.
# The nutritional content per unit of each food is shown in the table below. 
#
# Formulate a linear programming model that can be used to satisfy my daily 
# nutritional requirement at minimum cost.

# Type of                        Calories   Chocolate    Sugar    Fat
# Food                                      (ounces)     (ounces) (ounces)
# Chocolate Cake (1 slice)       400           3            2      2
# Chocolate ice cream (1 scoop)  200           2            2      4
# Cola (1 bottle)                150           0            4      1
# Pineapple cheesecake (1 piece) 500           0            4      5
# """

#
# This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
# See also my AMPL page: http://www.hakank.org/ampl/
#


##
## GMPL Model: Diet Problem
##
var x1>=0; # number of slices of chocolate cake eaten daily
var x2>=0; # number of scoops of chocolate ice cream eaten daily
var x3>=0; # number of bottles of cola drunk daily
var x4>=0; # number of pieces of pineapple cheesecake eaten daily
# cents per product
minimize DailyCost: 50*x1 + 20*x2 + 30*x3 + 80*x4;

subject to Calories: 400*x1 + 200*x2 + 150*x3 + 500*x4 >= 500;
subject to Chocolate: 3*x1 + 2*x2 >= 6;
subject to Sugar: 2*x1 + 2*x2 + 4*x3 + 4*x4 >= 10;
subject to Fat: 2*x1 + 4*x2 + x3 + 5*x4 >= 8;

solve;
display x1,x2,x3,x4;

end;

# glpsol --model diet.mod --output diet.output --wtxt diet.txt

# Solution (from diet.output)
# x1: 0   chocolate cake
# x2: 3   chocolate ice cream
# x3: 1   cola
# x4: 0   pineapple cheesecake

# DailyCost: 90
# Calories: 750
# In diet.txt there is detailed descriptions for variables,
# costs, etc.

