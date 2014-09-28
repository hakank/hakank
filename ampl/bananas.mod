/*
  """
  In three dollars, you get 5 bananas, in five dollars, 7 oranges, in
  seven dollars, 9 mangoes and in nine dollars, three apples, I need to
  purchase 100 fruits in 100 dollars. Please keep in mind that all type
  of fruits need to be purchased but I do not like banana and apple, so
  these should be of minimum quantity.
  """

  This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
  See also my AMPL page: http://www.hakank.org/ampl/


*/

var bananas integer >= 1;
var oranges integer >= 1;
var mangoes integer >= 1;
var apples integer >= 1;

minimize t: bananas+apples;

subject to a: 3*bananas/5 + 5*oranges/7 + 7*mangoes/9 + 9*apples/3 = 100;
subject to b: bananas + oranges + mangoes + apples = 100;

# option solver bonmin;
option solver cplex;
# option solver gecode;
solve;
display bananas, oranges, mangoes, apples;
display t;

end;
