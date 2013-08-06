/*
  From Xpress example
  sol1s3.html.mos

  http://www.chlond.demon.co.uk/puzzles/puzzles1.html
  Egg basket puzzle
  """
  3. A woman was carrying a basket of eggs to market when a passer-by bumped her. 
  She dropped the basket and all the eggs broke. The passer-by, wishing to pay 
  for her loss, asked, 'How many eggs were in your basket?'

  'I don't remember exactly,' the woman replied, 'but I do recall that whether 
  I divided the eggs by 2,3,4,5 or 6 there was always one egg left over. When 
  I took the eggs out in groups of seven, I emptied the basket.'

  What is the least number of eggs that broke? (Kordemsky)
  """


  Solution should be 301 eggs. And it is.

   This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
   See also my AMPL page: http://www.hakank.org/ampl/


*/

set INT = 1..6;
var x{INT} >= 0, integer;
var n >=0, integer;

# param  minnum := n; # what is this

subject to cona:
      n = 2*x[1]+1;

subject to conb:
     n = 3*x[2]+1;

subject to conc:
     n = 4*x[3]+1;

subject to cond :
      n = 5*x[4]+1;

subject to cone:
      n = 6*x[5]+1;

subject to conf:
     n = 7*x[6];

# forall(i in 1..6]
#    x[i] is_integer

# n is_integer

minimize minN:
         n;

option solver cplex;
solve;

display x;
display n;
