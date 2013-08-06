/*
In three dollars, you get 5 bananas, in five dollars, 7 oranges, in
seven dollars, 9 mangoes and in nine dollars, three apples, I need to
purchase 100 fruits in 100 dollars. Please keep in mind that all type
of fruits need to be purchased but I do not like banana and apple, so
these should be of minimum quantity."

Testing with gkpk:
$ glpsol --model bananas.mod 
Reading model section from bananas.mod...
23 lines were read
Generating t...
Generating a...
Generating b...
Model has been successfully generated
lpx_simplex: original LP has 3 rows, 4 columns, 10 non-zeros
lpx_simplex: presolved LP has 2 rows, 4 columns, 8 non-zeros
lpx_adv_basis: size of triangular part = 1
      0:   objval =   1.601798942e+02   infeas =   1.000000000e+00 (0)
      1:   objval =   9.800000000e+01   infeas =   0.000000000e+00 (0)
*     1:   objval =   9.800000000e+01   infeas =   0.000000000e+00 (0)
*     2:   objval =   1.110857143e+01   infeas =   0.000000000e+00 (0)
OPTIMAL SOLUTION FOUND
Integer optimization begins...
Objective function is integral
+     2: mip =     not found yet >=              -inf        (1; 0)
+   155: mip =   2.700000000e+01 >=   1.400000000e+01  48.1% (30; 132)
+   161: mip =   1.700000000e+01 >=   1.400000000e+01  17.6% (10; 249)
+   165: mip =   1.600000000e+01 >=   1.500000000e+01   6.2% (5; 269)
+   165: mip =   1.600000000e+01 >=     tree is empty   0.0% (0; 291)
INTEGER OPTIMAL SOLUTION FOUND
Time used:   0.0 secs
Memory used: 0.2M (158088 bytes)
Display statement at line 21
x = 5
y = 21
z = 63
w = 11
Model has been successfully processed


This AMPL model was created by Hakan Kjellerstrand, hakank@gmail.com
See also my AMPL page: http://www.hakank.org/ampl/


*/

var bananas integer >= 1;
var oranges integer >= 1;
var mangoes integer >= 1;
var apples integer >= 1;

minimize t: bananas+apples;
#maximize t: bananas+apples;

subject to a: 5*bananas/3 + 7*oranges/5 + 9*mangoes/7 + 3*apples/9 = 100;
subject to b: bananas + oranges + mangoes + apples = 100;

#option solver bonmin;
option solver cplex;
solve;
display bananas, oranges, mangoes, apples;

end;
